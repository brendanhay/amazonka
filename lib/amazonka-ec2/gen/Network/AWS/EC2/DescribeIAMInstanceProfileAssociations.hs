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
-- Module      : Network.AWS.EC2.DescribeIAMInstanceProfileAssociations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your IAM instance profile associations.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeIAMInstanceProfileAssociations
    (
    -- * Creating a Request
      describeIAMInstanceProfileAssociations
    , DescribeIAMInstanceProfileAssociations
    -- * Request Lenses
    , diapaFilters
    , diapaNextToken
    , diapaAssociationIds
    , diapaMaxResults

    -- * Destructuring the Response
    , describeIAMInstanceProfileAssociationsResponse
    , DescribeIAMInstanceProfileAssociationsResponse
    -- * Response Lenses
    , diaparsIAMInstanceProfileAssociations
    , diaparsNextToken
    , diaparsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeIAMInstanceProfileAssociations' smart constructor.
data DescribeIAMInstanceProfileAssociations = DescribeIAMInstanceProfileAssociations'
  { _diapaFilters        :: !(Maybe [Filter])
  , _diapaNextToken      :: !(Maybe Text)
  , _diapaAssociationIds :: !(Maybe [Text])
  , _diapaMaxResults     :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeIAMInstanceProfileAssociations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diapaFilters' - One or more filters.     * @instance-id@ - The ID of the instance.     * @state@ - The state of the association (@associating@ | @associated@ | @disassociating@ | @disassociated@ ).
--
-- * 'diapaNextToken' - The token to request the next page of results.
--
-- * 'diapaAssociationIds' - One or more IAM instance profile associations.
--
-- * 'diapaMaxResults' - The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value.
describeIAMInstanceProfileAssociations
    :: DescribeIAMInstanceProfileAssociations
describeIAMInstanceProfileAssociations =
  DescribeIAMInstanceProfileAssociations'
    { _diapaFilters = Nothing
    , _diapaNextToken = Nothing
    , _diapaAssociationIds = Nothing
    , _diapaMaxResults = Nothing
    }


-- | One or more filters.     * @instance-id@ - The ID of the instance.     * @state@ - The state of the association (@associating@ | @associated@ | @disassociating@ | @disassociated@ ).
diapaFilters :: Lens' DescribeIAMInstanceProfileAssociations [Filter]
diapaFilters = lens _diapaFilters (\ s a -> s{_diapaFilters = a}) . _Default . _Coerce

-- | The token to request the next page of results.
diapaNextToken :: Lens' DescribeIAMInstanceProfileAssociations (Maybe Text)
diapaNextToken = lens _diapaNextToken (\ s a -> s{_diapaNextToken = a})

-- | One or more IAM instance profile associations.
diapaAssociationIds :: Lens' DescribeIAMInstanceProfileAssociations [Text]
diapaAssociationIds = lens _diapaAssociationIds (\ s a -> s{_diapaAssociationIds = a}) . _Default . _Coerce

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value.
diapaMaxResults :: Lens' DescribeIAMInstanceProfileAssociations (Maybe Natural)
diapaMaxResults = lens _diapaMaxResults (\ s a -> s{_diapaMaxResults = a}) . mapping _Nat

instance AWSPager
           DescribeIAMInstanceProfileAssociations
         where
        page rq rs
          | stop (rs ^. diaparsNextToken) = Nothing
          | stop (rs ^. diaparsIAMInstanceProfileAssociations)
            = Nothing
          | otherwise =
            Just $ rq & diapaNextToken .~ rs ^. diaparsNextToken

instance AWSRequest
           DescribeIAMInstanceProfileAssociations
         where
        type Rs DescribeIAMInstanceProfileAssociations =
             DescribeIAMInstanceProfileAssociationsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeIAMInstanceProfileAssociationsResponse' <$>
                   (x .@? "iamInstanceProfileAssociationSet" .!@ mempty
                      >>= may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable
           DescribeIAMInstanceProfileAssociations
         where

instance NFData
           DescribeIAMInstanceProfileAssociations
         where

instance ToHeaders
           DescribeIAMInstanceProfileAssociations
         where
        toHeaders = const mempty

instance ToPath
           DescribeIAMInstanceProfileAssociations
         where
        toPath = const "/"

instance ToQuery
           DescribeIAMInstanceProfileAssociations
         where
        toQuery DescribeIAMInstanceProfileAssociations'{..}
          = mconcat
              ["Action" =:
                 ("DescribeIamInstanceProfileAssociations" ::
                    ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _diapaFilters),
               "NextToken" =: _diapaNextToken,
               toQuery
                 (toQueryList "AssociationId" <$>
                    _diapaAssociationIds),
               "MaxResults" =: _diapaMaxResults]

-- | /See:/ 'describeIAMInstanceProfileAssociationsResponse' smart constructor.
data DescribeIAMInstanceProfileAssociationsResponse = DescribeIAMInstanceProfileAssociationsResponse'
  { _diaparsIAMInstanceProfileAssociations :: !(Maybe [IAMInstanceProfileAssociation])
  , _diaparsNextToken :: !(Maybe Text)
  , _diaparsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeIAMInstanceProfileAssociationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diaparsIAMInstanceProfileAssociations' - Information about one or more IAM instance profile associations.
--
-- * 'diaparsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'diaparsResponseStatus' - -- | The response status code.
describeIAMInstanceProfileAssociationsResponse
    :: Int -- ^ 'diaparsResponseStatus'
    -> DescribeIAMInstanceProfileAssociationsResponse
describeIAMInstanceProfileAssociationsResponse pResponseStatus_ =
  DescribeIAMInstanceProfileAssociationsResponse'
    { _diaparsIAMInstanceProfileAssociations = Nothing
    , _diaparsNextToken = Nothing
    , _diaparsResponseStatus = pResponseStatus_
    }


-- | Information about one or more IAM instance profile associations.
diaparsIAMInstanceProfileAssociations :: Lens' DescribeIAMInstanceProfileAssociationsResponse [IAMInstanceProfileAssociation]
diaparsIAMInstanceProfileAssociations = lens _diaparsIAMInstanceProfileAssociations (\ s a -> s{_diaparsIAMInstanceProfileAssociations = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
diaparsNextToken :: Lens' DescribeIAMInstanceProfileAssociationsResponse (Maybe Text)
diaparsNextToken = lens _diaparsNextToken (\ s a -> s{_diaparsNextToken = a})

-- | -- | The response status code.
diaparsResponseStatus :: Lens' DescribeIAMInstanceProfileAssociationsResponse Int
diaparsResponseStatus = lens _diaparsResponseStatus (\ s a -> s{_diaparsResponseStatus = a})

instance NFData
           DescribeIAMInstanceProfileAssociationsResponse
         where
