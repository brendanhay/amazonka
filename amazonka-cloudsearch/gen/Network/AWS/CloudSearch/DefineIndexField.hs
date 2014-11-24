{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.DefineIndexField
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Configures an @'IndexField' for the search domain. Used to create new
-- fields and modify existing ones. You must specify the name of the domain
-- you are configuring and an index field configuration. The index field
-- configuration specifies a unique name, the index field type, and the
-- options you want to configure for the field. The options you can specify
-- depend on the @'IndexFieldType'. If the field exists, the new configuration
-- replaces the old one. For more information, see Configuring Index Fields in
-- the /Amazon CloudSearch Developer Guide/.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DefineIndexField.html>
module Network.AWS.CloudSearch.DefineIndexField
    (
    -- * Request
      DefineIndexField
    -- ** Request constructor
    , defineIndexField
    -- ** Request lenses
    , dif2DomainName
    , dif2IndexField

    -- * Response
    , DefineIndexFieldResponse
    -- ** Response constructor
    , defineIndexFieldResponse
    -- ** Response lenses
    , difr1IndexField
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types
import qualified GHC.Exts

data DefineIndexField = DefineIndexField
    { _dif2DomainName :: Text
    , _dif2IndexField :: IndexField
    } deriving (Eq, Show)

-- | 'DefineIndexField' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dif2DomainName' @::@ 'Text'
--
-- * 'dif2IndexField' @::@ 'IndexField'
--
defineIndexField :: Text -- ^ 'dif2DomainName'
                 -> IndexField -- ^ 'dif2IndexField'
                 -> DefineIndexField
defineIndexField p1 p2 = DefineIndexField
    { _dif2DomainName = p1
    , _dif2IndexField = p2
    }

dif2DomainName :: Lens' DefineIndexField Text
dif2DomainName = lens _dif2DomainName (\s a -> s { _dif2DomainName = a })

-- | The index field and field options you want to configure.
dif2IndexField :: Lens' DefineIndexField IndexField
dif2IndexField = lens _dif2IndexField (\s a -> s { _dif2IndexField = a })

newtype DefineIndexFieldResponse = DefineIndexFieldResponse
    { _difr1IndexField :: IndexFieldStatus
    } deriving (Eq, Show)

-- | 'DefineIndexFieldResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'difr1IndexField' @::@ 'IndexFieldStatus'
--
defineIndexFieldResponse :: IndexFieldStatus -- ^ 'difr1IndexField'
                         -> DefineIndexFieldResponse
defineIndexFieldResponse p1 = DefineIndexFieldResponse
    { _difr1IndexField = p1
    }

difr1IndexField :: Lens' DefineIndexFieldResponse IndexFieldStatus
difr1IndexField = lens _difr1IndexField (\s a -> s { _difr1IndexField = a })

instance ToPath DefineIndexField where
    toPath = const "/"

instance ToQuery DefineIndexField where
    toQuery DefineIndexField{..} = mconcat
        [ "DomainName" =? _dif2DomainName
        , "IndexField" =? _dif2IndexField
        ]

instance ToHeaders DefineIndexField

instance AWSRequest DefineIndexField where
    type Sv DefineIndexField = CloudSearch
    type Rs DefineIndexField = DefineIndexFieldResponse

    request  = post "DefineIndexField"
    response = xmlResponse

instance FromXML DefineIndexFieldResponse where
    parseXML = withElement "DefineIndexFieldResult" $ \x -> DefineIndexFieldResponse
        <$> x .@  "IndexField"
