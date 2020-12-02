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
-- Module      : Network.AWS.AutoScaling.SetInstanceProtection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the instance protection settings of the specified instances.
--
--
-- For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/as-instance-termination.html#instance-protection Instance Protection> in the /Auto Scaling User Guide/ .
--
module Network.AWS.AutoScaling.SetInstanceProtection
    (
    -- * Creating a Request
      setInstanceProtection
    , SetInstanceProtection
    -- * Request Lenses
    , sipInstanceIds
    , sipAutoScalingGroupName
    , sipProtectedFromScaleIn

    -- * Destructuring the Response
    , setInstanceProtectionResponse
    , SetInstanceProtectionResponse
    -- * Response Lenses
    , siprsResponseStatus
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'setInstanceProtection' smart constructor.
data SetInstanceProtection = SetInstanceProtection'
  { _sipInstanceIds          :: ![Text]
  , _sipAutoScalingGroupName :: !Text
  , _sipProtectedFromScaleIn :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetInstanceProtection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sipInstanceIds' - One or more instance IDs.
--
-- * 'sipAutoScalingGroupName' - The name of the Auto Scaling group.
--
-- * 'sipProtectedFromScaleIn' - Indicates whether the instance is protected from termination by Auto Scaling when scaling in.
setInstanceProtection
    :: Text -- ^ 'sipAutoScalingGroupName'
    -> Bool -- ^ 'sipProtectedFromScaleIn'
    -> SetInstanceProtection
setInstanceProtection pAutoScalingGroupName_ pProtectedFromScaleIn_ =
  SetInstanceProtection'
    { _sipInstanceIds = mempty
    , _sipAutoScalingGroupName = pAutoScalingGroupName_
    , _sipProtectedFromScaleIn = pProtectedFromScaleIn_
    }


-- | One or more instance IDs.
sipInstanceIds :: Lens' SetInstanceProtection [Text]
sipInstanceIds = lens _sipInstanceIds (\ s a -> s{_sipInstanceIds = a}) . _Coerce

-- | The name of the Auto Scaling group.
sipAutoScalingGroupName :: Lens' SetInstanceProtection Text
sipAutoScalingGroupName = lens _sipAutoScalingGroupName (\ s a -> s{_sipAutoScalingGroupName = a})

-- | Indicates whether the instance is protected from termination by Auto Scaling when scaling in.
sipProtectedFromScaleIn :: Lens' SetInstanceProtection Bool
sipProtectedFromScaleIn = lens _sipProtectedFromScaleIn (\ s a -> s{_sipProtectedFromScaleIn = a})

instance AWSRequest SetInstanceProtection where
        type Rs SetInstanceProtection =
             SetInstanceProtectionResponse
        request = postQuery autoScaling
        response
          = receiveXMLWrapper "SetInstanceProtectionResult"
              (\ s h x ->
                 SetInstanceProtectionResponse' <$>
                   (pure (fromEnum s)))

instance Hashable SetInstanceProtection where

instance NFData SetInstanceProtection where

instance ToHeaders SetInstanceProtection where
        toHeaders = const mempty

instance ToPath SetInstanceProtection where
        toPath = const "/"

instance ToQuery SetInstanceProtection where
        toQuery SetInstanceProtection'{..}
          = mconcat
              ["Action" =: ("SetInstanceProtection" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "InstanceIds" =:
                 toQueryList "member" _sipInstanceIds,
               "AutoScalingGroupName" =: _sipAutoScalingGroupName,
               "ProtectedFromScaleIn" =: _sipProtectedFromScaleIn]

-- | /See:/ 'setInstanceProtectionResponse' smart constructor.
newtype SetInstanceProtectionResponse = SetInstanceProtectionResponse'
  { _siprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetInstanceProtectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siprsResponseStatus' - -- | The response status code.
setInstanceProtectionResponse
    :: Int -- ^ 'siprsResponseStatus'
    -> SetInstanceProtectionResponse
setInstanceProtectionResponse pResponseStatus_ =
  SetInstanceProtectionResponse' {_siprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
siprsResponseStatus :: Lens' SetInstanceProtectionResponse Int
siprsResponseStatus = lens _siprsResponseStatus (\ s a -> s{_siprsResponseStatus = a})

instance NFData SetInstanceProtectionResponse where
