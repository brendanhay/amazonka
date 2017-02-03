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
-- Module      : Network.AWS.SSM.GetDefaultPatchBaseline
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the default patch baseline.
--
--
module Network.AWS.SSM.GetDefaultPatchBaseline
    (
    -- * Creating a Request
      getDefaultPatchBaseline
    , GetDefaultPatchBaseline

    -- * Destructuring the Response
    , getDefaultPatchBaselineResponse
    , GetDefaultPatchBaselineResponse
    -- * Response Lenses
    , gdpbrsBaselineId
    , gdpbrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SSM.Types
import           Network.AWS.SSM.Types.Product

-- | /See:/ 'getDefaultPatchBaseline' smart constructor.
data GetDefaultPatchBaseline =
    GetDefaultPatchBaseline'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetDefaultPatchBaseline' with the minimum fields required to make a request.
--
getDefaultPatchBaseline
    :: GetDefaultPatchBaseline
getDefaultPatchBaseline = GetDefaultPatchBaseline'

instance AWSRequest GetDefaultPatchBaseline where
        type Rs GetDefaultPatchBaseline =
             GetDefaultPatchBaselineResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 GetDefaultPatchBaselineResponse' <$>
                   (x .?> "BaselineId") <*> (pure (fromEnum s)))

instance Hashable GetDefaultPatchBaseline

instance NFData GetDefaultPatchBaseline

instance ToHeaders GetDefaultPatchBaseline where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.GetDefaultPatchBaseline" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDefaultPatchBaseline where
        toJSON = const (Object mempty)

instance ToPath GetDefaultPatchBaseline where
        toPath = const "/"

instance ToQuery GetDefaultPatchBaseline where
        toQuery = const mempty

-- | /See:/ 'getDefaultPatchBaselineResponse' smart constructor.
data GetDefaultPatchBaselineResponse = GetDefaultPatchBaselineResponse'
    { _gdpbrsBaselineId     :: !(Maybe Text)
    , _gdpbrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetDefaultPatchBaselineResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdpbrsBaselineId' - The ID of the default patch baseline.
--
-- * 'gdpbrsResponseStatus' - -- | The response status code.
getDefaultPatchBaselineResponse
    :: Int -- ^ 'gdpbrsResponseStatus'
    -> GetDefaultPatchBaselineResponse
getDefaultPatchBaselineResponse pResponseStatus_ =
    GetDefaultPatchBaselineResponse'
    { _gdpbrsBaselineId = Nothing
    , _gdpbrsResponseStatus = pResponseStatus_
    }

-- | The ID of the default patch baseline.
gdpbrsBaselineId :: Lens' GetDefaultPatchBaselineResponse (Maybe Text)
gdpbrsBaselineId = lens _gdpbrsBaselineId (\ s a -> s{_gdpbrsBaselineId = a});

-- | -- | The response status code.
gdpbrsResponseStatus :: Lens' GetDefaultPatchBaselineResponse Int
gdpbrsResponseStatus = lens _gdpbrsResponseStatus (\ s a -> s{_gdpbrsResponseStatus = a});

instance NFData GetDefaultPatchBaselineResponse
