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

-- Module      : Network.AWS.Glacier.DeleteVault
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation deletes a vault. Amazon Glacier will delete a vault only if
-- there are no archives in the vault as of the last inventory and there have
-- been no writes to the vault since the last inventory. If either of these
-- conditions is not satisfied, the vault deletion fails (that is, the vault is
-- not removed) and Amazon Glacier returns an error. You can use 'DescribeVault'
-- to return the number of archives in a vault, and you can use <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-initiate-job-post.html Initiate a Job(POST jobs)> to initiate a new inventory retrieval for a vault. The inventory
-- contains the archive IDs you use to delete archives using <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-archive-delete.html Delete Archive(DELETE archive)>.
--
-- This operation is idempotent.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don't have any
-- permissions by default. You must grant them explicit permission to perform
-- specific actions. For more information, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identityand Access Management (IAM)>.
--
-- For conceptual information and underlying REST API, go to <http://docs.aws.amazon.com/amazonglacier/latest/dev/deleting-vaults.html Deleting a Vaultin Amazon Glacier> and <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-delete.html Delete Vault > in the /Amazon Glacier Developer Guide/.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-DeleteVault.html>
module Network.AWS.Glacier.DeleteVault
    (
    -- * Request
      DeleteVault
    -- ** Request constructor
    , deleteVault
    -- ** Request lenses
    , dv1AccountId
    , dv1VaultName

    -- * Response
    , DeleteVaultResponse
    -- ** Response constructor
    , deleteVaultResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Glacier.Types
import qualified GHC.Exts

data DeleteVault = DeleteVault
    { _dv1AccountId :: Text
    , _dv1VaultName :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DeleteVault' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dv1AccountId' @::@ 'Text'
--
-- * 'dv1VaultName' @::@ 'Text'
--
deleteVault :: Text -- ^ 'dv1AccountId'
            -> Text -- ^ 'dv1VaultName'
            -> DeleteVault
deleteVault p1 p2 = DeleteVault
    { _dv1AccountId = p1
    , _dv1VaultName = p2
    }

-- | The 'AccountId' is the AWS Account ID. You can specify either the AWS Account
-- ID or optionally a '-', in which case Amazon Glacier uses the AWS Account ID
-- associated with the credentials used to sign the request. If you specify your
-- Account ID, do not include hyphens in it.
dv1AccountId :: Lens' DeleteVault Text
dv1AccountId = lens _dv1AccountId (\s a -> s { _dv1AccountId = a })

-- | The name of the vault.
dv1VaultName :: Lens' DeleteVault Text
dv1VaultName = lens _dv1VaultName (\s a -> s { _dv1VaultName = a })

data DeleteVaultResponse = DeleteVaultResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteVaultResponse' constructor.
deleteVaultResponse :: DeleteVaultResponse
deleteVaultResponse = DeleteVaultResponse

instance ToPath DeleteVault where
    toPath DeleteVault{..} = mconcat
        [ "/"
        , toText _dv1AccountId
        , "/vaults/"
        , toText _dv1VaultName
        ]

instance ToQuery DeleteVault where
    toQuery = const mempty

instance ToHeaders DeleteVault

instance ToJSON DeleteVault where
    toJSON = const (toJSON Empty)

instance AWSRequest DeleteVault where
    type Sv DeleteVault = Glacier
    type Rs DeleteVault = DeleteVaultResponse

    request  = delete
    response = nullResponse DeleteVaultResponse
