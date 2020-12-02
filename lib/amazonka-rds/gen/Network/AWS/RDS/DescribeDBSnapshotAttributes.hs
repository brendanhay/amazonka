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
-- Module      : Network.AWS.RDS.DescribeDBSnapshotAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of DB snapshot attribute names and values for a manual DB snapshot.
--
--
-- When sharing snapshots with other AWS accounts, @DescribeDBSnapshotAttributes@ returns the @restore@ attribute and a list of IDs for the AWS accounts that are authorized to copy or restore the manual DB snapshot. If @all@ is included in the list of values for the @restore@ attribute, then the manual DB snapshot is public and can be copied or restored by all AWS accounts.
--
-- To add or remove access for an AWS account to copy or restore a manual DB snapshot, or to make the manual DB snapshot public or private, use the 'ModifyDBSnapshotAttribute' API action.
--
module Network.AWS.RDS.DescribeDBSnapshotAttributes
    (
    -- * Creating a Request
      describeDBSnapshotAttributes
    , DescribeDBSnapshotAttributes
    -- * Request Lenses
    , ddsaDBSnapshotIdentifier

    -- * Destructuring the Response
    , describeDBSnapshotAttributesResponse
    , DescribeDBSnapshotAttributesResponse
    -- * Response Lenses
    , ddsarsDBSnapshotAttributesResult
    , ddsarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeDBSnapshotAttributes' smart constructor.
newtype DescribeDBSnapshotAttributes = DescribeDBSnapshotAttributes'
  { _ddsaDBSnapshotIdentifier :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDBSnapshotAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddsaDBSnapshotIdentifier' - The identifier for the DB snapshot to describe the attributes for.
describeDBSnapshotAttributes
    :: Text -- ^ 'ddsaDBSnapshotIdentifier'
    -> DescribeDBSnapshotAttributes
describeDBSnapshotAttributes pDBSnapshotIdentifier_ =
  DescribeDBSnapshotAttributes'
    {_ddsaDBSnapshotIdentifier = pDBSnapshotIdentifier_}


-- | The identifier for the DB snapshot to describe the attributes for.
ddsaDBSnapshotIdentifier :: Lens' DescribeDBSnapshotAttributes Text
ddsaDBSnapshotIdentifier = lens _ddsaDBSnapshotIdentifier (\ s a -> s{_ddsaDBSnapshotIdentifier = a})

instance AWSRequest DescribeDBSnapshotAttributes
         where
        type Rs DescribeDBSnapshotAttributes =
             DescribeDBSnapshotAttributesResponse
        request = postQuery rds
        response
          = receiveXMLWrapper
              "DescribeDBSnapshotAttributesResult"
              (\ s h x ->
                 DescribeDBSnapshotAttributesResponse' <$>
                   (x .@? "DBSnapshotAttributesResult") <*>
                     (pure (fromEnum s)))

instance Hashable DescribeDBSnapshotAttributes where

instance NFData DescribeDBSnapshotAttributes where

instance ToHeaders DescribeDBSnapshotAttributes where
        toHeaders = const mempty

instance ToPath DescribeDBSnapshotAttributes where
        toPath = const "/"

instance ToQuery DescribeDBSnapshotAttributes where
        toQuery DescribeDBSnapshotAttributes'{..}
          = mconcat
              ["Action" =:
                 ("DescribeDBSnapshotAttributes" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "DBSnapshotIdentifier" =: _ddsaDBSnapshotIdentifier]

-- | /See:/ 'describeDBSnapshotAttributesResponse' smart constructor.
data DescribeDBSnapshotAttributesResponse = DescribeDBSnapshotAttributesResponse'
  { _ddsarsDBSnapshotAttributesResult :: !(Maybe DBSnapshotAttributesResult)
  , _ddsarsResponseStatus             :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDBSnapshotAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddsarsDBSnapshotAttributesResult' - Undocumented member.
--
-- * 'ddsarsResponseStatus' - -- | The response status code.
describeDBSnapshotAttributesResponse
    :: Int -- ^ 'ddsarsResponseStatus'
    -> DescribeDBSnapshotAttributesResponse
describeDBSnapshotAttributesResponse pResponseStatus_ =
  DescribeDBSnapshotAttributesResponse'
    { _ddsarsDBSnapshotAttributesResult = Nothing
    , _ddsarsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
ddsarsDBSnapshotAttributesResult :: Lens' DescribeDBSnapshotAttributesResponse (Maybe DBSnapshotAttributesResult)
ddsarsDBSnapshotAttributesResult = lens _ddsarsDBSnapshotAttributesResult (\ s a -> s{_ddsarsDBSnapshotAttributesResult = a})

-- | -- | The response status code.
ddsarsResponseStatus :: Lens' DescribeDBSnapshotAttributesResponse Int
ddsarsResponseStatus = lens _ddsarsResponseStatus (\ s a -> s{_ddsarsResponseStatus = a})

instance NFData DescribeDBSnapshotAttributesResponse
         where
