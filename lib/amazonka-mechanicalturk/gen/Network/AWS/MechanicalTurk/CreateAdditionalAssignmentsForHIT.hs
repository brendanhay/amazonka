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
-- Module      : Network.AWS.MechanicalTurk.CreateAdditionalAssignmentsForHIT
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @CreateAdditionalAssignmentsForHIT@ operation increases the maximum number of assignments of an existing HIT.
--
--
-- To extend the maximum number of assignments, specify the number of additional assignments.
--
module Network.AWS.MechanicalTurk.CreateAdditionalAssignmentsForHIT
    (
    -- * Creating a Request
      createAdditionalAssignmentsForHIT
    , CreateAdditionalAssignmentsForHIT
    -- * Request Lenses
    , caafhitUniqueRequestToken
    , caafhitHITId
    , caafhitNumberOfAdditionalAssignments

    -- * Destructuring the Response
    , createAdditionalAssignmentsForHITResponse
    , CreateAdditionalAssignmentsForHITResponse
    -- * Response Lenses
    , caafhitrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types
import Network.AWS.MechanicalTurk.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createAdditionalAssignmentsForHIT' smart constructor.
data CreateAdditionalAssignmentsForHIT = CreateAdditionalAssignmentsForHIT'
  { _caafhitUniqueRequestToken            :: !(Maybe Text)
  , _caafhitHITId                         :: !Text
  , _caafhitNumberOfAdditionalAssignments :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAdditionalAssignmentsForHIT' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caafhitUniqueRequestToken' - A unique identifier for this request, which allows you to retry the call on error without extending the HIT multiple times. This is useful in cases such as network timeouts where it is unclear whether or not the call succeeded on the server. If the extend HIT already exists in the system from a previous call using the same @UniqueRequestToken@ , subsequent calls will return an error with a message containing the request ID.
--
-- * 'caafhitHITId' - The ID of the HIT to extend.
--
-- * 'caafhitNumberOfAdditionalAssignments' - The number of additional assignments to request for this HIT.
createAdditionalAssignmentsForHIT
    :: Text -- ^ 'caafhitHITId'
    -> Int -- ^ 'caafhitNumberOfAdditionalAssignments'
    -> CreateAdditionalAssignmentsForHIT
createAdditionalAssignmentsForHIT pHITId_ pNumberOfAdditionalAssignments_ =
  CreateAdditionalAssignmentsForHIT'
    { _caafhitUniqueRequestToken = Nothing
    , _caafhitHITId = pHITId_
    , _caafhitNumberOfAdditionalAssignments = pNumberOfAdditionalAssignments_
    }


-- | A unique identifier for this request, which allows you to retry the call on error without extending the HIT multiple times. This is useful in cases such as network timeouts where it is unclear whether or not the call succeeded on the server. If the extend HIT already exists in the system from a previous call using the same @UniqueRequestToken@ , subsequent calls will return an error with a message containing the request ID.
caafhitUniqueRequestToken :: Lens' CreateAdditionalAssignmentsForHIT (Maybe Text)
caafhitUniqueRequestToken = lens _caafhitUniqueRequestToken (\ s a -> s{_caafhitUniqueRequestToken = a})

-- | The ID of the HIT to extend.
caafhitHITId :: Lens' CreateAdditionalAssignmentsForHIT Text
caafhitHITId = lens _caafhitHITId (\ s a -> s{_caafhitHITId = a})

-- | The number of additional assignments to request for this HIT.
caafhitNumberOfAdditionalAssignments :: Lens' CreateAdditionalAssignmentsForHIT Int
caafhitNumberOfAdditionalAssignments = lens _caafhitNumberOfAdditionalAssignments (\ s a -> s{_caafhitNumberOfAdditionalAssignments = a})

instance AWSRequest CreateAdditionalAssignmentsForHIT
         where
        type Rs CreateAdditionalAssignmentsForHIT =
             CreateAdditionalAssignmentsForHITResponse
        request = postJSON mechanicalTurk
        response
          = receiveEmpty
              (\ s h x ->
                 CreateAdditionalAssignmentsForHITResponse' <$>
                   (pure (fromEnum s)))

instance Hashable CreateAdditionalAssignmentsForHIT
         where

instance NFData CreateAdditionalAssignmentsForHIT
         where

instance ToHeaders CreateAdditionalAssignmentsForHIT
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MTurkRequesterServiceV20170117.CreateAdditionalAssignmentsForHIT"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateAdditionalAssignmentsForHIT
         where
        toJSON CreateAdditionalAssignmentsForHIT'{..}
          = object
              (catMaybes
                 [("UniqueRequestToken" .=) <$>
                    _caafhitUniqueRequestToken,
                  Just ("HITId" .= _caafhitHITId),
                  Just
                    ("NumberOfAdditionalAssignments" .=
                       _caafhitNumberOfAdditionalAssignments)])

instance ToPath CreateAdditionalAssignmentsForHIT
         where
        toPath = const "/"

instance ToQuery CreateAdditionalAssignmentsForHIT
         where
        toQuery = const mempty

-- | /See:/ 'createAdditionalAssignmentsForHITResponse' smart constructor.
newtype CreateAdditionalAssignmentsForHITResponse = CreateAdditionalAssignmentsForHITResponse'
  { _caafhitrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAdditionalAssignmentsForHITResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caafhitrsResponseStatus' - -- | The response status code.
createAdditionalAssignmentsForHITResponse
    :: Int -- ^ 'caafhitrsResponseStatus'
    -> CreateAdditionalAssignmentsForHITResponse
createAdditionalAssignmentsForHITResponse pResponseStatus_ =
  CreateAdditionalAssignmentsForHITResponse'
    {_caafhitrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
caafhitrsResponseStatus :: Lens' CreateAdditionalAssignmentsForHITResponse Int
caafhitrsResponseStatus = lens _caafhitrsResponseStatus (\ s a -> s{_caafhitrsResponseStatus = a})

instance NFData
           CreateAdditionalAssignmentsForHITResponse
         where
