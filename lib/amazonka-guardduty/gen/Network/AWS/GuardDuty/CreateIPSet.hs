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
    , cisLocation
    , cisFormat
    , cisActivate
    , cisName
    , cisDetectorId

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
  { _cisLocation   :: !(Maybe Text)
  , _cisFormat     :: !(Maybe IPSetFormat)
  , _cisActivate   :: !(Maybe Bool)
  , _cisName       :: !(Maybe Text)
  , _cisDetectorId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateIPSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cisLocation' - The URI of the file that contains the IPSet. For example (https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key)
--
-- * 'cisFormat' - The format of the file that contains the IPSet.
--
-- * 'cisActivate' - A boolean value that indicates whether GuardDuty is to start using the uploaded IPSet.
--
-- * 'cisName' - The user friendly name to identify the IPSet. This name is displayed in all findings that are triggered by activity that involves IP addresses included in this IPSet.
--
-- * 'cisDetectorId' - The unique ID of the detector that you want to update.
createIPSet
    :: Text -- ^ 'cisDetectorId'
    -> CreateIPSet
createIPSet pDetectorId_ =
  CreateIPSet'
    { _cisLocation = Nothing
    , _cisFormat = Nothing
    , _cisActivate = Nothing
    , _cisName = Nothing
    , _cisDetectorId = pDetectorId_
    }


-- | The URI of the file that contains the IPSet. For example (https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key)
cisLocation :: Lens' CreateIPSet (Maybe Text)
cisLocation = lens _cisLocation (\ s a -> s{_cisLocation = a})

-- | The format of the file that contains the IPSet.
cisFormat :: Lens' CreateIPSet (Maybe IPSetFormat)
cisFormat = lens _cisFormat (\ s a -> s{_cisFormat = a})

-- | A boolean value that indicates whether GuardDuty is to start using the uploaded IPSet.
cisActivate :: Lens' CreateIPSet (Maybe Bool)
cisActivate = lens _cisActivate (\ s a -> s{_cisActivate = a})

-- | The user friendly name to identify the IPSet. This name is displayed in all findings that are triggered by activity that involves IP addresses included in this IPSet.
cisName :: Lens' CreateIPSet (Maybe Text)
cisName = lens _cisName (\ s a -> s{_cisName = a})

-- | The unique ID of the detector that you want to update.
cisDetectorId :: Lens' CreateIPSet Text
cisDetectorId = lens _cisDetectorId (\ s a -> s{_cisDetectorId = a})

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
                 [("location" .=) <$> _cisLocation,
                  ("format" .=) <$> _cisFormat,
                  ("activate" .=) <$> _cisActivate,
                  ("name" .=) <$> _cisName])

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
