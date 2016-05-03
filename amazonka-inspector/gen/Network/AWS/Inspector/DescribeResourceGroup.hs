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
-- Module      : Network.AWS.Inspector.DescribeResourceGroup
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the resource group specified by the resource group ARN.
module Network.AWS.Inspector.DescribeResourceGroup
    (
    -- * Creating a Request
      describeResourceGroup
    , DescribeResourceGroup
    -- * Request Lenses
    , drgResourceGroupARN

    -- * Destructuring the Response
    , describeResourceGroupResponse
    , DescribeResourceGroupResponse
    -- * Response Lenses
    , drgrsResourceGroup
    , drgrsResponseStatus
    ) where

import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeResourceGroup' smart constructor.
newtype DescribeResourceGroup = DescribeResourceGroup'
    { _drgResourceGroupARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeResourceGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drgResourceGroupARN'
describeResourceGroup
    :: Text -- ^ 'drgResourceGroupARN'
    -> DescribeResourceGroup
describeResourceGroup pResourceGroupARN_ =
    DescribeResourceGroup'
    { _drgResourceGroupARN = pResourceGroupARN_
    }

-- | The ARN specifying the resource group that you want to describe.
drgResourceGroupARN :: Lens' DescribeResourceGroup Text
drgResourceGroupARN = lens _drgResourceGroupARN (\ s a -> s{_drgResourceGroupARN = a});

instance AWSRequest DescribeResourceGroup where
        type Rs DescribeResourceGroup =
             DescribeResourceGroupResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 DescribeResourceGroupResponse' <$>
                   (x .?> "resourceGroup") <*> (pure (fromEnum s)))

instance Hashable DescribeResourceGroup

instance NFData DescribeResourceGroup

instance ToHeaders DescribeResourceGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.DescribeResourceGroup" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeResourceGroup where
        toJSON DescribeResourceGroup'{..}
          = object
              (catMaybes
                 [Just ("resourceGroupArn" .= _drgResourceGroupARN)])

instance ToPath DescribeResourceGroup where
        toPath = const "/"

instance ToQuery DescribeResourceGroup where
        toQuery = const mempty

-- | /See:/ 'describeResourceGroupResponse' smart constructor.
data DescribeResourceGroupResponse = DescribeResourceGroupResponse'
    { _drgrsResourceGroup  :: !(Maybe ResourceGroup)
    , _drgrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeResourceGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drgrsResourceGroup'
--
-- * 'drgrsResponseStatus'
describeResourceGroupResponse
    :: Int -- ^ 'drgrsResponseStatus'
    -> DescribeResourceGroupResponse
describeResourceGroupResponse pResponseStatus_ =
    DescribeResourceGroupResponse'
    { _drgrsResourceGroup = Nothing
    , _drgrsResponseStatus = pResponseStatus_
    }

-- | Information about the resource group.
drgrsResourceGroup :: Lens' DescribeResourceGroupResponse (Maybe ResourceGroup)
drgrsResourceGroup = lens _drgrsResourceGroup (\ s a -> s{_drgrsResourceGroup = a});

-- | The response status code.
drgrsResponseStatus :: Lens' DescribeResourceGroupResponse Int
drgrsResponseStatus = lens _drgrsResponseStatus (\ s a -> s{_drgrsResponseStatus = a});

instance NFData DescribeResourceGroupResponse
