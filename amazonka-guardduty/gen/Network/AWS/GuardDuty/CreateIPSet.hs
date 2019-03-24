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
-- Module      : Network.AWS.GuardDuty.CreateIPSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new IPSet - a list of trusted IP addresses that have been whitelisted for secure communication with AWS infrastructure and applications.
module Network.AWS.GuardDuty.CreateIPSet
    (
    -- * Creating a Request
      createIPSet
    , CreateIPSet
    -- * Request Lenses
    , cisClientToken
    , cisDetectorId
    , cisFormat
    , cisActivate
    , cisLocation
    , cisName

    -- * Destructuring the Response
    , createIPSetResponse
    , CreateIPSetResponse
    -- * Response Lenses
    , cisrsIPSetId
    , cisrsResponseStatus
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | CreateIPSet request body.
--
-- /See:/ 'createIPSet' smart constructor.
data CreateIPSet = CreateIPSet'
  { _cisClientToken :: !(Maybe Text)
  , _cisDetectorId  :: !Text
  , _cisFormat      :: !IPSetFormat
  , _cisActivate    :: !Bool
  , _cisLocation    :: !Text
  , _cisName        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateIPSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cisClientToken' - The idempotency token for the create request.
--
-- * 'cisDetectorId' - The unique ID of the detector that you want to update.
--
-- * 'cisFormat' - The format of the file that contains the IPSet.
--
-- * 'cisActivate' - A boolean value that indicates whether GuardDuty is to start using the uploaded IPSet.
--
-- * 'cisLocation' - The URI of the file that contains the IPSet. For example (https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key)
--
-- * 'cisName' - The user friendly name to identify the IPSet. This name is displayed in all findings that are triggered by activity that involves IP addresses included in this IPSet.
createIPSet
    :: Text -- ^ 'cisDetectorId'
    -> IPSetFormat -- ^ 'cisFormat'
    -> Bool -- ^ 'cisActivate'
    -> Text -- ^ 'cisLocation'
    -> Text -- ^ 'cisName'
    -> CreateIPSet
createIPSet pDetectorId_ pFormat_ pActivate_ pLocation_ pName_ =
  CreateIPSet'
    { _cisClientToken = Nothing
    , _cisDetectorId = pDetectorId_
    , _cisFormat = pFormat_
    , _cisActivate = pActivate_
    , _cisLocation = pLocation_
    , _cisName = pName_
    }


-- | The idempotency token for the create request.
cisClientToken :: Lens' CreateIPSet (Maybe Text)
cisClientToken = lens _cisClientToken (\ s a -> s{_cisClientToken = a})

-- | The unique ID of the detector that you want to update.
cisDetectorId :: Lens' CreateIPSet Text
cisDetectorId = lens _cisDetectorId (\ s a -> s{_cisDetectorId = a})

-- | The format of the file that contains the IPSet.
cisFormat :: Lens' CreateIPSet IPSetFormat
cisFormat = lens _cisFormat (\ s a -> s{_cisFormat = a})

-- | A boolean value that indicates whether GuardDuty is to start using the uploaded IPSet.
cisActivate :: Lens' CreateIPSet Bool
cisActivate = lens _cisActivate (\ s a -> s{_cisActivate = a})

-- | The URI of the file that contains the IPSet. For example (https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key)
cisLocation :: Lens' CreateIPSet Text
cisLocation = lens _cisLocation (\ s a -> s{_cisLocation = a})

-- | The user friendly name to identify the IPSet. This name is displayed in all findings that are triggered by activity that involves IP addresses included in this IPSet.
cisName :: Lens' CreateIPSet Text
cisName = lens _cisName (\ s a -> s{_cisName = a})

instance AWSRequest CreateIPSet where
        type Rs CreateIPSet = CreateIPSetResponse
        request = postJSON guardDuty
        response
          = receiveJSON
              (\ s h x ->
                 CreateIPSetResponse' <$>
                   (x .?> "ipSetId") <*> (pure (fromEnum s)))

instance Hashable CreateIPSet where

instance NFData CreateIPSet where

instance ToHeaders CreateIPSet where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateIPSet where
        toJSON CreateIPSet'{..}
          = object
              (catMaybes
                 [("clientToken" .=) <$> _cisClientToken,
                  Just ("format" .= _cisFormat),
                  Just ("activate" .= _cisActivate),
                  Just ("location" .= _cisLocation),
                  Just ("name" .= _cisName)])

instance ToPath CreateIPSet where
        toPath CreateIPSet'{..}
          = mconcat
              ["/detector/", toBS _cisDetectorId, "/ipset"]

instance ToQuery CreateIPSet where
        toQuery = const mempty

-- | /See:/ 'createIPSetResponse' smart constructor.
data CreateIPSetResponse = CreateIPSetResponse'
  { _cisrsIPSetId        :: !(Maybe Text)
  , _cisrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateIPSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cisrsIPSetId' - Undocumented member.
--
-- * 'cisrsResponseStatus' - -- | The response status code.
createIPSetResponse
    :: Int -- ^ 'cisrsResponseStatus'
    -> CreateIPSetResponse
createIPSetResponse pResponseStatus_ =
  CreateIPSetResponse'
    {_cisrsIPSetId = Nothing, _cisrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
cisrsIPSetId :: Lens' CreateIPSetResponse (Maybe Text)
cisrsIPSetId = lens _cisrsIPSetId (\ s a -> s{_cisrsIPSetId = a})

-- | -- | The response status code.
cisrsResponseStatus :: Lens' CreateIPSetResponse Int
cisrsResponseStatus = lens _cisrsResponseStatus (\ s a -> s{_cisrsResponseStatus = a})

instance NFData CreateIPSetResponse where
