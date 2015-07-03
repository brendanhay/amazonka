{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Glacier.DescribeVault
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation returns information about a vault, including the vault\'s
-- Amazon Resource Name (ARN), the date the vault was created, the number
-- of archives it contains, and the total size of all the archives in the
-- vault. The number of archives and their total size are as of the last
-- inventory generation. This means that if you add or remove an archive
-- from a vault, and then immediately use Describe Vault, the change in
-- contents will not be immediately reflected. If you want to retrieve the
-- latest inventory of the vault, use InitiateJob. Amazon Glacier generates
-- vault inventories approximately daily. For more information, see
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/vault-inventory.html Downloading a Vault Inventory in Amazon Glacier>.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don\'t have any
-- permissions by default. You must grant them explicit permission to
-- perform specific actions. For more information, see
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)>.
--
-- For conceptual information and underlying REST API, go to
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/retrieving-vault-info.html Retrieving Vault Metadata in Amazon Glacier>
-- and
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-get.html Describe Vault>
-- in the /Amazon Glacier Developer Guide/.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-DescribeVault.html>
module Network.AWS.Glacier.DescribeVault
    (
    -- * Request
      DescribeVault
    -- ** Request constructor
    , describeVault
    -- ** Request lenses
    , dvAccountId
    , dvVaultName

    -- * Response
    , DescribeVaultOutput
    -- ** Response constructor
    , describeVaultOutput
    -- ** Response lenses
    , dvoVaultName
    , dvoSizeInBytes
    , dvoLastInventoryDate
    , dvoVaultARN
    , dvoCreationDate
    , dvoNumberOfArchives
    ) where

import           Network.AWS.Glacier.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Provides options for retrieving metadata for a specific vault in Amazon
-- Glacier.
--
-- /See:/ 'describeVault' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvAccountId'
--
-- * 'dvVaultName'
data DescribeVault = DescribeVault'
    { _dvAccountId :: !Text
    , _dvVaultName :: !Text
    } deriving (Eq,Read,Show)

-- | 'DescribeVault' smart constructor.
describeVault :: Text -> Text -> DescribeVault
describeVault pAccountId pVaultName =
    DescribeVault'
    { _dvAccountId = pAccountId
    , _dvVaultName = pVaultName
    }

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (apos-apos) in the ID.
dvAccountId :: Lens' DescribeVault Text
dvAccountId = lens _dvAccountId (\ s a -> s{_dvAccountId = a});

-- | The name of the vault.
dvVaultName :: Lens' DescribeVault Text
dvVaultName = lens _dvVaultName (\ s a -> s{_dvVaultName = a});

instance AWSRequest DescribeVault where
        type Sv DescribeVault = Glacier
        type Rs DescribeVault = DescribeVaultOutput
        request = get
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders DescribeVault where
        toHeaders = const mempty

instance ToPath DescribeVault where
        toPath DescribeVault'{..}
          = mconcat
              ["/", toText _dvAccountId, "/vaults/",
               toText _dvVaultName]

instance ToQuery DescribeVault where
        toQuery = const mempty
