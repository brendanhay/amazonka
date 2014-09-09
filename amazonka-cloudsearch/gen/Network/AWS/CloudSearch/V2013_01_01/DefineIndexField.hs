{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.V2013_01_01.DefineIndexField
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Configures an IndexField for the search domain. Used to create new fields
-- and modify existing ones. You must specify the name of the domain you are
-- configuring and an index field configuration. The index field configuration
-- specifies a unique name, the index field type, and the options you want to
-- configure for the field. The options you can specify depend on the
-- IndexFieldType. If the field exists, the new configuration replaces the old
-- one. For more information, see Configuring Index Fields in the Amazon
-- CloudSearch Developer Guide.
module Network.AWS.CloudSearch.V2013_01_01.DefineIndexField
    (
    -- * Request
      DefineIndexField
    -- ** Request constructor
    , mkDefineIndexField
    -- ** Request lenses
    , difDomainName
    , difIndexField

    -- * Response
    , DefineIndexFieldResponse
    -- ** Response constructor
    , mkDefineIndexFieldResponse
    -- ** Response lenses
    , difrIndexField
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Container for the parameters to the DefineIndexField operation. Specifies
-- the name of the domain you want to update and the index field
-- configuration.
data DefineIndexField = DefineIndexField
    { _difDomainName :: Text
    , _difIndexField :: IndexField
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DefineIndexField' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DomainName ::@ @Text@
--
-- * @IndexField ::@ @IndexField@
--
mkDefineIndexField :: Text -- ^ 'difDomainName'
                   -> IndexField -- ^ 'difIndexField'
                   -> DefineIndexField
mkDefineIndexField p1 p2 = DefineIndexField
    { _difDomainName = p1
    , _difIndexField = p2
    }

-- | A string that represents the name of a domain. Domain names are unique
-- across the domains owned by an account within an AWS region. Domain names
-- start with a letter or number and can contain the following characters: a-z
-- (lowercase), 0-9, and - (hyphen).
difDomainName :: Lens' DefineIndexField Text
difDomainName = lens _difDomainName (\s a -> s { _difDomainName = a })

-- | The index field and field options you want to configure.
difIndexField :: Lens' DefineIndexField IndexField
difIndexField = lens _difIndexField (\s a -> s { _difIndexField = a })

instance ToQuery DefineIndexField where
    toQuery = genericQuery def

-- | The result of a DefineIndexField request. Contains the status of the
-- newly-configured index field.
newtype DefineIndexFieldResponse = DefineIndexFieldResponse
    { _difrIndexField :: IndexFieldStatus
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DefineIndexFieldResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @IndexField ::@ @IndexFieldStatus@
--
mkDefineIndexFieldResponse :: IndexFieldStatus -- ^ 'difrIndexField'
                           -> DefineIndexFieldResponse
mkDefineIndexFieldResponse p1 = DefineIndexFieldResponse
    { _difrIndexField = p1
    }

-- | The value of an IndexField and its current status.
difrIndexField :: Lens' DefineIndexFieldResponse IndexFieldStatus
difrIndexField = lens _difrIndexField (\s a -> s { _difrIndexField = a })

instance FromXML DefineIndexFieldResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DefineIndexField where
    type Sv DefineIndexField = CloudSearch
    type Rs DefineIndexField = DefineIndexFieldResponse

    request = post "DefineIndexField"
    response _ = xmlResponse
