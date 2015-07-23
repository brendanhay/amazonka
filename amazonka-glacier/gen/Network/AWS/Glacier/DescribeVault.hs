{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.DescribeVault
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation returns information about a vault, including the vault\'s
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
    , dvrqAccountId
    , dvrqVaultName

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
-- * 'dvrqAccountId'
--
-- * 'dvrqVaultName'
data DescribeVault = DescribeVault'
    { _dvrqAccountId :: !Text
    , _dvrqVaultName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVault' smart constructor.
describeVault :: Text -> Text -> DescribeVault
describeVault pAccountId_ pVaultName_ =
    DescribeVault'
    { _dvrqAccountId = pAccountId_
    , _dvrqVaultName = pVaultName_
    }

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (apos-apos) in the ID.
dvrqAccountId :: Lens' DescribeVault Text
dvrqAccountId = lens _dvrqAccountId (\ s a -> s{_dvrqAccountId = a});

-- | The name of the vault.
dvrqVaultName :: Lens' DescribeVault Text
dvrqVaultName = lens _dvrqVaultName (\ s a -> s{_dvrqVaultName = a});

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
              ["/", toText _dvrqAccountId, "/vaults/",
               toText _dvrqVaultName]

instance ToQuery DescribeVault where
        toQuery = const mempty
