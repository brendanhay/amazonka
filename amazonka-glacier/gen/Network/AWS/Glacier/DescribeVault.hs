{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.DescribeVault
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns information about a vault, including the vault's Amazon Resource Name (ARN), the date the vault was created, the number of archives it contains, and the total size of all the archives in the vault. The number of archives and their total size are as of the last inventory generation. This means that if you add or remove an archive from a vault, and then immediately use Describe Vault, the change in contents will not be immediately reflected. If you want to retrieve the latest inventory of the vault, use 'InitiateJob' . Amazon Glacier generates vault inventories approximately daily. For more information, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-inventory.html Downloading a Vault Inventory in Amazon Glacier> .
--
--
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
--
-- For conceptual information and underlying REST API, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/retrieving-vault-info.html Retrieving Vault Metadata in Amazon Glacier> and <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-get.html Describe Vault > in the /Amazon Glacier Developer Guide/ .
--
module Network.AWS.Glacier.DescribeVault
    (
    -- * Creating a Request
      describeVault
    , DescribeVault
    -- * Request Lenses
    , dvAccountId
    , dvVaultName

    -- * Destructuring the Response
    , describeVaultOutput
    , DescribeVaultOutput
    -- * Response Lenses
    , dvoVaultName
    , dvoSizeInBytes
    , dvoLastInventoryDate
    , dvoVaultARN
    , dvoCreationDate
    , dvoNumberOfArchives
    ) where

import Network.AWS.Glacier.Types
import Network.AWS.Glacier.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Provides options for retrieving metadata for a specific vault in Amazon Glacier.
--
--
--
-- /See:/ 'describeVault' smart constructor.
data DescribeVault = DescribeVault'
  { _dvAccountId :: !Text
  , _dvVaultName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVault' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvAccountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- * 'dvVaultName' - The name of the vault.
describeVault
    :: Text -- ^ 'dvAccountId'
    -> Text -- ^ 'dvVaultName'
    -> DescribeVault
describeVault pAccountId_ pVaultName_ =
  DescribeVault' {_dvAccountId = pAccountId_, _dvVaultName = pVaultName_}


-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
dvAccountId :: Lens' DescribeVault Text
dvAccountId = lens _dvAccountId (\ s a -> s{_dvAccountId = a})

-- | The name of the vault.
dvVaultName :: Lens' DescribeVault Text
dvVaultName = lens _dvVaultName (\ s a -> s{_dvVaultName = a})

instance AWSRequest DescribeVault where
        type Rs DescribeVault = DescribeVaultOutput
        request = get glacier
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable DescribeVault where

instance NFData DescribeVault where

instance ToHeaders DescribeVault where
        toHeaders = const mempty

instance ToPath DescribeVault where
        toPath DescribeVault'{..}
          = mconcat
              ["/", toBS _dvAccountId, "/vaults/",
               toBS _dvVaultName]

instance ToQuery DescribeVault where
        toQuery = const mempty
