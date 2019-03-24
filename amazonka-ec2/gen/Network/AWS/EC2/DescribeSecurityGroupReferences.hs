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
-- Module      : Network.AWS.EC2.DescribeSecurityGroupReferences
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- [EC2-VPC only] Describes the VPCs on the other side of a VPC peering connection that are referencing the security groups you've specified in this request.
--
--
module Network.AWS.EC2.DescribeSecurityGroupReferences
    (
    -- * Creating a Request
      describeSecurityGroupReferences
    , DescribeSecurityGroupReferences
    -- * Request Lenses
    , dsgrDryRun
    , dsgrGroupId

    -- * Destructuring the Response
    , describeSecurityGroupReferencesResponse
    , DescribeSecurityGroupReferencesResponse
    -- * Response Lenses
    , dsgrrsSecurityGroupReferenceSet
    , dsgrrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeSecurityGroupReferences' smart constructor.
data DescribeSecurityGroupReferences = DescribeSecurityGroupReferences'
  { _dsgrDryRun  :: !(Maybe Bool)
  , _dsgrGroupId :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSecurityGroupReferences' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsgrDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dsgrGroupId' - One or more security group IDs in your account.
describeSecurityGroupReferences
    :: DescribeSecurityGroupReferences
describeSecurityGroupReferences =
  DescribeSecurityGroupReferences'
    {_dsgrDryRun = Nothing, _dsgrGroupId = mempty}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dsgrDryRun :: Lens' DescribeSecurityGroupReferences (Maybe Bool)
dsgrDryRun = lens _dsgrDryRun (\ s a -> s{_dsgrDryRun = a})

-- | One or more security group IDs in your account.
dsgrGroupId :: Lens' DescribeSecurityGroupReferences [Text]
dsgrGroupId = lens _dsgrGroupId (\ s a -> s{_dsgrGroupId = a}) . _Coerce

instance AWSRequest DescribeSecurityGroupReferences
         where
        type Rs DescribeSecurityGroupReferences =
             DescribeSecurityGroupReferencesResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeSecurityGroupReferencesResponse' <$>
                   (x .@? "securityGroupReferenceSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeSecurityGroupReferences
         where

instance NFData DescribeSecurityGroupReferences where

instance ToHeaders DescribeSecurityGroupReferences
         where
        toHeaders = const mempty

instance ToPath DescribeSecurityGroupReferences where
        toPath = const "/"

instance ToQuery DescribeSecurityGroupReferences
         where
        toQuery DescribeSecurityGroupReferences'{..}
          = mconcat
              ["Action" =:
                 ("DescribeSecurityGroupReferences" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dsgrDryRun,
               toQueryList "GroupId" _dsgrGroupId]

-- | /See:/ 'describeSecurityGroupReferencesResponse' smart constructor.
data DescribeSecurityGroupReferencesResponse = DescribeSecurityGroupReferencesResponse'
  { _dsgrrsSecurityGroupReferenceSet :: !(Maybe [SecurityGroupReference])
  , _dsgrrsResponseStatus            :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSecurityGroupReferencesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsgrrsSecurityGroupReferenceSet' - Information about the VPCs with the referencing security groups.
--
-- * 'dsgrrsResponseStatus' - -- | The response status code.
describeSecurityGroupReferencesResponse
    :: Int -- ^ 'dsgrrsResponseStatus'
    -> DescribeSecurityGroupReferencesResponse
describeSecurityGroupReferencesResponse pResponseStatus_ =
  DescribeSecurityGroupReferencesResponse'
    { _dsgrrsSecurityGroupReferenceSet = Nothing
    , _dsgrrsResponseStatus = pResponseStatus_
    }


-- | Information about the VPCs with the referencing security groups.
dsgrrsSecurityGroupReferenceSet :: Lens' DescribeSecurityGroupReferencesResponse [SecurityGroupReference]
dsgrrsSecurityGroupReferenceSet = lens _dsgrrsSecurityGroupReferenceSet (\ s a -> s{_dsgrrsSecurityGroupReferenceSet = a}) . _Default . _Coerce

-- | -- | The response status code.
dsgrrsResponseStatus :: Lens' DescribeSecurityGroupReferencesResponse Int
dsgrrsResponseStatus = lens _dsgrrsResponseStatus (\ s a -> s{_dsgrrsResponseStatus = a})

instance NFData
           DescribeSecurityGroupReferencesResponse
         where
