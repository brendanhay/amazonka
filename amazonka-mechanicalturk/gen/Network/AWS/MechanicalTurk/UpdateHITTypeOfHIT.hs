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
-- Module      : Network.AWS.MechanicalTurk.UpdateHITTypeOfHIT
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @UpdateHITTypeOfHIT@ operation allows you to change the HITType properties of a HIT. This operation disassociates the HIT from its old HITType properties and associates it with the new HITType properties. The HIT takes on the properties of the new HITType in place of the old ones.
--
--
module Network.AWS.MechanicalTurk.UpdateHITTypeOfHIT
    (
    -- * Creating a Request
      updateHITTypeOfHIT
    , UpdateHITTypeOfHIT
    -- * Request Lenses
    , uhittohitHITId
    , uhittohitHITTypeId

    -- * Destructuring the Response
    , updateHITTypeOfHITResponse
    , UpdateHITTypeOfHITResponse
    -- * Response Lenses
    , uhittohitrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types
import Network.AWS.MechanicalTurk.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateHITTypeOfHIT' smart constructor.
data UpdateHITTypeOfHIT = UpdateHITTypeOfHIT'
  { _uhittohitHITId     :: !Text
  , _uhittohitHITTypeId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateHITTypeOfHIT' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uhittohitHITId' - The HIT to update.
--
-- * 'uhittohitHITTypeId' - The ID of the new HIT type.
updateHITTypeOfHIT
    :: Text -- ^ 'uhittohitHITId'
    -> Text -- ^ 'uhittohitHITTypeId'
    -> UpdateHITTypeOfHIT
updateHITTypeOfHIT pHITId_ pHITTypeId_ =
  UpdateHITTypeOfHIT'
    {_uhittohitHITId = pHITId_, _uhittohitHITTypeId = pHITTypeId_}


-- | The HIT to update.
uhittohitHITId :: Lens' UpdateHITTypeOfHIT Text
uhittohitHITId = lens _uhittohitHITId (\ s a -> s{_uhittohitHITId = a})

-- | The ID of the new HIT type.
uhittohitHITTypeId :: Lens' UpdateHITTypeOfHIT Text
uhittohitHITTypeId = lens _uhittohitHITTypeId (\ s a -> s{_uhittohitHITTypeId = a})

instance AWSRequest UpdateHITTypeOfHIT where
        type Rs UpdateHITTypeOfHIT =
             UpdateHITTypeOfHITResponse
        request = postJSON mechanicalTurk
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateHITTypeOfHITResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateHITTypeOfHIT where

instance NFData UpdateHITTypeOfHIT where

instance ToHeaders UpdateHITTypeOfHIT where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MTurkRequesterServiceV20170117.UpdateHITTypeOfHIT"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateHITTypeOfHIT where
        toJSON UpdateHITTypeOfHIT'{..}
          = object
              (catMaybes
                 [Just ("HITId" .= _uhittohitHITId),
                  Just ("HITTypeId" .= _uhittohitHITTypeId)])

instance ToPath UpdateHITTypeOfHIT where
        toPath = const "/"

instance ToQuery UpdateHITTypeOfHIT where
        toQuery = const mempty

-- | /See:/ 'updateHITTypeOfHITResponse' smart constructor.
newtype UpdateHITTypeOfHITResponse = UpdateHITTypeOfHITResponse'
  { _uhittohitrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateHITTypeOfHITResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uhittohitrsResponseStatus' - -- | The response status code.
updateHITTypeOfHITResponse
    :: Int -- ^ 'uhittohitrsResponseStatus'
    -> UpdateHITTypeOfHITResponse
updateHITTypeOfHITResponse pResponseStatus_ =
  UpdateHITTypeOfHITResponse' {_uhittohitrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
uhittohitrsResponseStatus :: Lens' UpdateHITTypeOfHITResponse Int
uhittohitrsResponseStatus = lens _uhittohitrsResponseStatus (\ s a -> s{_uhittohitrsResponseStatus = a})

instance NFData UpdateHITTypeOfHITResponse where
