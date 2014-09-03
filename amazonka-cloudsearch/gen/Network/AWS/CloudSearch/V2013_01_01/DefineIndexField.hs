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
    , defineIndexField
    -- ** Request lenses
    , difrDomainName
    , difrIndexField

    -- * Response
    , DefineIndexFieldResponse
    -- ** Response lenses
    , difsIndexField
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DefineIndexField' request.
defineIndexField :: Text -- ^ 'difrDomainName'
                 -> IndexField -- ^ 'difrIndexField'
                 -> DefineIndexField
defineIndexField p1 p2 = DefineIndexField
    { _difrDomainName = p1
    , _difrIndexField = p2
    }

data DefineIndexField = DefineIndexField
    { _difrDomainName :: Text
      -- ^ A string that represents the name of a domain. Domain names are
      -- unique across the domains owned by an account within an AWS
      -- region. Domain names start with a letter or number and can
      -- contain the following characters: a-z (lowercase), 0-9, and -
      -- (hyphen).
    , _difrIndexField :: IndexField
      -- ^ The index field and field options you want to configure.
    } deriving (Show, Generic)

-- | A string that represents the name of a domain. Domain names are unique
-- across the domains owned by an account within an AWS region. Domain names
-- start with a letter or number and can contain the following characters: a-z
-- (lowercase), 0-9, and - (hyphen).
difrDomainName
    :: Functor f
    => (Text
    -> f (Text))
    -> DefineIndexField
    -> f DefineIndexField
difrDomainName f x =
    (\y -> x { _difrDomainName = y })
       <$> f (_difrDomainName x)
{-# INLINE difrDomainName #-}

-- | The index field and field options you want to configure.
difrIndexField
    :: Functor f
    => (IndexField
    -> f (IndexField))
    -> DefineIndexField
    -> f DefineIndexField
difrIndexField f x =
    (\y -> x { _difrIndexField = y })
       <$> f (_difrIndexField x)
{-# INLINE difrIndexField #-}

instance ToQuery DefineIndexField where
    toQuery = genericQuery def

data DefineIndexFieldResponse = DefineIndexFieldResponse
    { _difsIndexField :: IndexFieldStatus
      -- ^ The value of an IndexField and its current status.
    } deriving (Show, Generic)

-- | The value of an IndexField and its current status.
difsIndexField
    :: Functor f
    => (IndexFieldStatus
    -> f (IndexFieldStatus))
    -> DefineIndexFieldResponse
    -> f DefineIndexFieldResponse
difsIndexField f x =
    (\y -> x { _difsIndexField = y })
       <$> f (_difsIndexField x)
{-# INLINE difsIndexField #-}

instance FromXML DefineIndexFieldResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DefineIndexField where
    type Sv DefineIndexField = CloudSearch
    type Rs DefineIndexField = DefineIndexFieldResponse

    request = post "DefineIndexField"
    response _ = xmlResponse
