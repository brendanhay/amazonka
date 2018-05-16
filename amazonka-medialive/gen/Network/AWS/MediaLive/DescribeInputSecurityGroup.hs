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
-- Module      : Network.AWS.MediaLive.DescribeInputSecurityGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Produces a summary of an Input Security Group
module Network.AWS.MediaLive.DescribeInputSecurityGroup
    (
    -- * Creating a Request
      describeInputSecurityGroup
    , DescribeInputSecurityGroup
    -- * Request Lenses
    , disgInputSecurityGroupId

    -- * Destructuring the Response
    , describeInputSecurityGroupResponse
    , DescribeInputSecurityGroupResponse
    -- * Response Lenses
    , desrsState
    , desrsARN
    , desrsInputs
    , desrsId
    , desrsWhitelistRules
    , desrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.MediaLive.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for DescribeInputSecurityGroupRequest
--
-- /See:/ 'describeInputSecurityGroup' smart constructor.
newtype DescribeInputSecurityGroup = DescribeInputSecurityGroup'
  { _disgInputSecurityGroupId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeInputSecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'disgInputSecurityGroupId' - The id of the Input Security Group to describe
describeInputSecurityGroup
    :: Text -- ^ 'disgInputSecurityGroupId'
    -> DescribeInputSecurityGroup
describeInputSecurityGroup pInputSecurityGroupId_ =
  DescribeInputSecurityGroup'
    {_disgInputSecurityGroupId = pInputSecurityGroupId_}


-- | The id of the Input Security Group to describe
disgInputSecurityGroupId :: Lens' DescribeInputSecurityGroup Text
disgInputSecurityGroupId = lens _disgInputSecurityGroupId (\ s a -> s{_disgInputSecurityGroupId = a})

instance AWSRequest DescribeInputSecurityGroup where
        type Rs DescribeInputSecurityGroup =
             DescribeInputSecurityGroupResponse
        request = get mediaLive
        response
          = receiveJSON
              (\ s h x ->
                 DescribeInputSecurityGroupResponse' <$>
                   (x .?> "state") <*> (x .?> "arn") <*>
                     (x .?> "inputs" .!@ mempty)
                     <*> (x .?> "id")
                     <*> (x .?> "whitelistRules" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeInputSecurityGroup where

instance NFData DescribeInputSecurityGroup where

instance ToHeaders DescribeInputSecurityGroup where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeInputSecurityGroup where
        toPath DescribeInputSecurityGroup'{..}
          = mconcat
              ["/prod/inputSecurityGroups/",
               toBS _disgInputSecurityGroupId]

instance ToQuery DescribeInputSecurityGroup where
        toQuery = const mempty

-- | Placeholder documentation for DescribeInputSecurityGroupResponse
--
-- /See:/ 'describeInputSecurityGroupResponse' smart constructor.
data DescribeInputSecurityGroupResponse = DescribeInputSecurityGroupResponse'
  { _desrsState          :: !(Maybe InputSecurityGroupState)
  , _desrsARN            :: !(Maybe Text)
  , _desrsInputs         :: !(Maybe [Text])
  , _desrsId             :: !(Maybe Text)
  , _desrsWhitelistRules :: !(Maybe [InputWhitelistRule])
  , _desrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeInputSecurityGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desrsState' - The current state of the Input Security Group.
--
-- * 'desrsARN' - Unique ARN of Input Security Group
--
-- * 'desrsInputs' - The list of inputs currently using this Input Security Group.
--
-- * 'desrsId' - The Id of the Input Security Group
--
-- * 'desrsWhitelistRules' - Whitelist rules and their sync status
--
-- * 'desrsResponseStatus' - -- | The response status code.
describeInputSecurityGroupResponse
    :: Int -- ^ 'desrsResponseStatus'
    -> DescribeInputSecurityGroupResponse
describeInputSecurityGroupResponse pResponseStatus_ =
  DescribeInputSecurityGroupResponse'
    { _desrsState = Nothing
    , _desrsARN = Nothing
    , _desrsInputs = Nothing
    , _desrsId = Nothing
    , _desrsWhitelistRules = Nothing
    , _desrsResponseStatus = pResponseStatus_
    }


-- | The current state of the Input Security Group.
desrsState :: Lens' DescribeInputSecurityGroupResponse (Maybe InputSecurityGroupState)
desrsState = lens _desrsState (\ s a -> s{_desrsState = a})

-- | Unique ARN of Input Security Group
desrsARN :: Lens' DescribeInputSecurityGroupResponse (Maybe Text)
desrsARN = lens _desrsARN (\ s a -> s{_desrsARN = a})

-- | The list of inputs currently using this Input Security Group.
desrsInputs :: Lens' DescribeInputSecurityGroupResponse [Text]
desrsInputs = lens _desrsInputs (\ s a -> s{_desrsInputs = a}) . _Default . _Coerce

-- | The Id of the Input Security Group
desrsId :: Lens' DescribeInputSecurityGroupResponse (Maybe Text)
desrsId = lens _desrsId (\ s a -> s{_desrsId = a})

-- | Whitelist rules and their sync status
desrsWhitelistRules :: Lens' DescribeInputSecurityGroupResponse [InputWhitelistRule]
desrsWhitelistRules = lens _desrsWhitelistRules (\ s a -> s{_desrsWhitelistRules = a}) . _Default . _Coerce

-- | -- | The response status code.
desrsResponseStatus :: Lens' DescribeInputSecurityGroupResponse Int
desrsResponseStatus = lens _desrsResponseStatus (\ s a -> s{_desrsResponseStatus = a})

instance NFData DescribeInputSecurityGroupResponse
         where
