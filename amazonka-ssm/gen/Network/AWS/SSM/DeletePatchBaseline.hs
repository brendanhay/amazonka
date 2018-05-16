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
-- Module      : Network.AWS.SSM.DeletePatchBaseline
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a patch baseline.
--
--
module Network.AWS.SSM.DeletePatchBaseline
    (
    -- * Creating a Request
      deletePatchBaseline
    , DeletePatchBaseline
    -- * Request Lenses
    , dpbBaselineId

    -- * Destructuring the Response
    , deletePatchBaselineResponse
    , DeletePatchBaselineResponse
    -- * Response Lenses
    , dpbrsBaselineId
    , dpbrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'deletePatchBaseline' smart constructor.
newtype DeletePatchBaseline = DeletePatchBaseline'
  { _dpbBaselineId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePatchBaseline' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpbBaselineId' - The ID of the patch baseline to delete.
deletePatchBaseline
    :: Text -- ^ 'dpbBaselineId'
    -> DeletePatchBaseline
deletePatchBaseline pBaselineId_ =
  DeletePatchBaseline' {_dpbBaselineId = pBaselineId_}


-- | The ID of the patch baseline to delete.
dpbBaselineId :: Lens' DeletePatchBaseline Text
dpbBaselineId = lens _dpbBaselineId (\ s a -> s{_dpbBaselineId = a})

instance AWSRequest DeletePatchBaseline where
        type Rs DeletePatchBaseline =
             DeletePatchBaselineResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 DeletePatchBaselineResponse' <$>
                   (x .?> "BaselineId") <*> (pure (fromEnum s)))

instance Hashable DeletePatchBaseline where

instance NFData DeletePatchBaseline where

instance ToHeaders DeletePatchBaseline where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DeletePatchBaseline" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeletePatchBaseline where
        toJSON DeletePatchBaseline'{..}
          = object
              (catMaybes [Just ("BaselineId" .= _dpbBaselineId)])

instance ToPath DeletePatchBaseline where
        toPath = const "/"

instance ToQuery DeletePatchBaseline where
        toQuery = const mempty

-- | /See:/ 'deletePatchBaselineResponse' smart constructor.
data DeletePatchBaselineResponse = DeletePatchBaselineResponse'
  { _dpbrsBaselineId     :: !(Maybe Text)
  , _dpbrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePatchBaselineResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpbrsBaselineId' - The ID of the deleted patch baseline.
--
-- * 'dpbrsResponseStatus' - -- | The response status code.
deletePatchBaselineResponse
    :: Int -- ^ 'dpbrsResponseStatus'
    -> DeletePatchBaselineResponse
deletePatchBaselineResponse pResponseStatus_ =
  DeletePatchBaselineResponse'
    {_dpbrsBaselineId = Nothing, _dpbrsResponseStatus = pResponseStatus_}


-- | The ID of the deleted patch baseline.
dpbrsBaselineId :: Lens' DeletePatchBaselineResponse (Maybe Text)
dpbrsBaselineId = lens _dpbrsBaselineId (\ s a -> s{_dpbrsBaselineId = a})

-- | -- | The response status code.
dpbrsResponseStatus :: Lens' DeletePatchBaselineResponse Int
dpbrsResponseStatus = lens _dpbrsResponseStatus (\ s a -> s{_dpbrsResponseStatus = a})

instance NFData DeletePatchBaselineResponse where
