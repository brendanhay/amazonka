{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.DeleteAccountAlias
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
module Network.AWS.IAM.V2010_05_08.DeleteAccountAlias
    (
    -- * Request
      DeleteAccountAlias
    -- ** Request constructor
    , deleteAccountAlias
    -- ** Request lenses
    , daarAccountAlias

    -- * Response
    , DeleteAccountAliasResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteAccountAlias' request.
deleteAccountAlias :: Text -- ^ 'daarAccountAlias'
                   -> DeleteAccountAlias
deleteAccountAlias p1 = DeleteAccountAlias
    { _daarAccountAlias = p1
    }
{-# INLINE deleteAccountAlias #-}

data DeleteAccountAlias = DeleteAccountAlias
    { _daarAccountAlias :: Text
      -- ^ Name of the account alias to delete.
    } deriving (Show, Generic)

-- | Name of the account alias to delete.
daarAccountAlias :: Lens' DeleteAccountAlias (Text)
daarAccountAlias f x =
    f (_daarAccountAlias x)
        <&> \y -> x { _daarAccountAlias = y }
{-# INLINE daarAccountAlias #-}

instance ToQuery DeleteAccountAlias where
    toQuery = genericQuery def

data DeleteAccountAliasResponse = DeleteAccountAliasResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteAccountAlias where
    type Sv DeleteAccountAlias = IAM
    type Rs DeleteAccountAlias = DeleteAccountAliasResponse

    request = post "DeleteAccountAlias"
    response _ = nullaryResponse DeleteAccountAliasResponse
