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
-- Module      : Network.AWS.XRay.UpdateSamplingRule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a sampling rule's configuration.
--
--
module Network.AWS.XRay.UpdateSamplingRule
    (
    -- * Creating a Request
      updateSamplingRule
    , UpdateSamplingRule
    -- * Request Lenses
    , usrSamplingRuleUpdate

    -- * Destructuring the Response
    , updateSamplingRuleResponse
    , UpdateSamplingRuleResponse
    -- * Response Lenses
    , usrrsSamplingRuleRecord
    , usrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.XRay.Types
import Network.AWS.XRay.Types.Product

-- | /See:/ 'updateSamplingRule' smart constructor.
newtype UpdateSamplingRule = UpdateSamplingRule'
  { _usrSamplingRuleUpdate :: SamplingRuleUpdate
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateSamplingRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usrSamplingRuleUpdate' - The rule and fields to change.
updateSamplingRule
    :: SamplingRuleUpdate -- ^ 'usrSamplingRuleUpdate'
    -> UpdateSamplingRule
updateSamplingRule pSamplingRuleUpdate_ =
  UpdateSamplingRule' {_usrSamplingRuleUpdate = pSamplingRuleUpdate_}


-- | The rule and fields to change.
usrSamplingRuleUpdate :: Lens' UpdateSamplingRule SamplingRuleUpdate
usrSamplingRuleUpdate = lens _usrSamplingRuleUpdate (\ s a -> s{_usrSamplingRuleUpdate = a})

instance AWSRequest UpdateSamplingRule where
        type Rs UpdateSamplingRule =
             UpdateSamplingRuleResponse
        request = postJSON xRay
        response
          = receiveJSON
              (\ s h x ->
                 UpdateSamplingRuleResponse' <$>
                   (x .?> "SamplingRuleRecord") <*> (pure (fromEnum s)))

instance Hashable UpdateSamplingRule where

instance NFData UpdateSamplingRule where

instance ToHeaders UpdateSamplingRule where
        toHeaders = const mempty

instance ToJSON UpdateSamplingRule where
        toJSON UpdateSamplingRule'{..}
          = object
              (catMaybes
                 [Just
                    ("SamplingRuleUpdate" .= _usrSamplingRuleUpdate)])

instance ToPath UpdateSamplingRule where
        toPath = const "/UpdateSamplingRule"

instance ToQuery UpdateSamplingRule where
        toQuery = const mempty

-- | /See:/ 'updateSamplingRuleResponse' smart constructor.
data UpdateSamplingRuleResponse = UpdateSamplingRuleResponse'
  { _usrrsSamplingRuleRecord :: !(Maybe SamplingRuleRecord)
  , _usrrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateSamplingRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usrrsSamplingRuleRecord' - The updated rule definition and metadata.
--
-- * 'usrrsResponseStatus' - -- | The response status code.
updateSamplingRuleResponse
    :: Int -- ^ 'usrrsResponseStatus'
    -> UpdateSamplingRuleResponse
updateSamplingRuleResponse pResponseStatus_ =
  UpdateSamplingRuleResponse'
    { _usrrsSamplingRuleRecord = Nothing
    , _usrrsResponseStatus = pResponseStatus_
    }


-- | The updated rule definition and metadata.
usrrsSamplingRuleRecord :: Lens' UpdateSamplingRuleResponse (Maybe SamplingRuleRecord)
usrrsSamplingRuleRecord = lens _usrrsSamplingRuleRecord (\ s a -> s{_usrrsSamplingRuleRecord = a})

-- | -- | The response status code.
usrrsResponseStatus :: Lens' UpdateSamplingRuleResponse Int
usrrsResponseStatus = lens _usrrsResponseStatus (\ s a -> s{_usrrsResponseStatus = a})

instance NFData UpdateSamplingRuleResponse where
