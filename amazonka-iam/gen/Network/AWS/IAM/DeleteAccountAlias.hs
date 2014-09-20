{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.DeleteAccountAlias
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified AWS account alias. For information about using an AWS
-- account alias, see Using an Alias for Your AWS Account ID in the Using IAM
-- guide. https://iam.amazonaws.com/ ?Action=DeleteAccountAlias
-- &AccountAlias=foocorporation &Version=2010-05-08 &AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.DeleteAccountAlias
    (
    -- * Request
      DeleteAccountAlias
    -- ** Request constructor
    , deleteAccountAlias
    -- ** Request lenses
    , daaAccountAlias

    -- * Response
    , DeleteAccountAliasResponse
    -- ** Response constructor
    , deleteAccountAliasResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

newtype DeleteAccountAlias = DeleteAccountAlias
    { _daaAccountAlias :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteAccountAlias' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AccountAlias ::@ @Text@
--
deleteAccountAlias :: Text -- ^ 'daaAccountAlias'
                   -> DeleteAccountAlias
deleteAccountAlias p1 = DeleteAccountAlias
    { _daaAccountAlias = p1
    }

-- | Name of the account alias to delete.
daaAccountAlias :: Lens' DeleteAccountAlias Text
daaAccountAlias = lens _daaAccountAlias (\s a -> s { _daaAccountAlias = a })

instance ToQuery DeleteAccountAlias where
    toQuery = genericQuery def

data DeleteAccountAliasResponse = DeleteAccountAliasResponse
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteAccountAliasResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deleteAccountAliasResponse :: DeleteAccountAliasResponse
deleteAccountAliasResponse = DeleteAccountAliasResponse

instance AWSRequest DeleteAccountAlias where
    type Sv DeleteAccountAlias = IAM
    type Rs DeleteAccountAlias = DeleteAccountAliasResponse

    request = post "DeleteAccountAlias"
    response _ = nullaryResponse DeleteAccountAliasResponse
