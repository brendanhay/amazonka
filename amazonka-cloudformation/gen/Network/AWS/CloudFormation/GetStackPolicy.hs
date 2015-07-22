{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.GetStackPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns the stack policy for a specified stack. If a stack doesn\'t have
-- a policy, a null value is returned.
--
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_GetStackPolicy.html>
module Network.AWS.CloudFormation.GetStackPolicy
    (
    -- * Request
      GetStackPolicy
    -- ** Request constructor
    , getStackPolicy
    -- ** Request lenses
    , gsprqStackName

    -- * Response
    , GetStackPolicyResponse
    -- ** Response constructor
    , getStackPolicyResponse
    -- ** Response lenses
    , gsprsStackPolicyBody
    , gsprsStatus
    ) where

import           Network.AWS.CloudFormation.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the GetStackPolicy action.
--
-- /See:/ 'getStackPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsprqStackName'
newtype GetStackPolicy = GetStackPolicy'
    { _gsprqStackName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetStackPolicy' smart constructor.
getStackPolicy :: Text -> GetStackPolicy
getStackPolicy pStackName =
    GetStackPolicy'
    { _gsprqStackName = pStackName
    }

-- | The name or unique stack ID that is associated with the stack whose
-- policy you want to get.
gsprqStackName :: Lens' GetStackPolicy Text
gsprqStackName = lens _gsprqStackName (\ s a -> s{_gsprqStackName = a});

instance AWSRequest GetStackPolicy where
        type Sv GetStackPolicy = CloudFormation
        type Rs GetStackPolicy = GetStackPolicyResponse
        request = post
        response
          = receiveXMLWrapper "GetStackPolicyResult"
              (\ s h x ->
                 GetStackPolicyResponse' <$>
                   (x .@? "StackPolicyBody") <*> (pure (fromEnum s)))

instance ToHeaders GetStackPolicy where
        toHeaders = const mempty

instance ToPath GetStackPolicy where
        toPath = const "/"

instance ToQuery GetStackPolicy where
        toQuery GetStackPolicy'{..}
          = mconcat
              ["Action" =: ("GetStackPolicy" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "StackName" =: _gsprqStackName]

-- | The output for the GetStackPolicy action.
--
-- /See:/ 'getStackPolicyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsprsStackPolicyBody'
--
-- * 'gsprsStatus'
data GetStackPolicyResponse = GetStackPolicyResponse'
    { _gsprsStackPolicyBody :: !(Maybe Text)
    , _gsprsStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetStackPolicyResponse' smart constructor.
getStackPolicyResponse :: Int -> GetStackPolicyResponse
getStackPolicyResponse pStatus =
    GetStackPolicyResponse'
    { _gsprsStackPolicyBody = Nothing
    , _gsprsStatus = pStatus
    }

-- | Structure containing the stack policy body. (For more information, go to
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources>
-- in the AWS CloudFormation User Guide.)
gsprsStackPolicyBody :: Lens' GetStackPolicyResponse (Maybe Text)
gsprsStackPolicyBody = lens _gsprsStackPolicyBody (\ s a -> s{_gsprsStackPolicyBody = a});

-- | FIXME: Undocumented member.
gsprsStatus :: Lens' GetStackPolicyResponse Int
gsprsStatus = lens _gsprsStatus (\ s a -> s{_gsprsStatus = a});
