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
-- Module      : Network.AWS.MediaLive.UpdateInput
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an input.
module Network.AWS.MediaLive.UpdateInput
    (
    -- * Creating a Request
      updateInput
    , UpdateInput
    -- * Request Lenses
    , uiSources
    , uiInputSecurityGroups
    , uiDestinations
    , uiName
    , uiInputId

    -- * Destructuring the Response
    , updateInputResponse
    , UpdateInputResponse
    -- * Response Lenses
    , uirsInput
    , uirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.MediaLive.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request to update an input.
--
-- /See:/ 'updateInput' smart constructor.
data UpdateInput = UpdateInput'
  { _uiSources             :: !(Maybe [InputSourceRequest])
  , _uiInputSecurityGroups :: !(Maybe [Text])
  , _uiDestinations        :: !(Maybe [InputDestinationRequest])
  , _uiName                :: !(Maybe Text)
  , _uiInputId             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiSources' - The source URLs for a PULL-type input. Every PULL type input needs exactly two source URLs for redundancy. Only specify sources for PULL type Inputs. Leave Destinations empty.
--
-- * 'uiInputSecurityGroups' - A list of security groups referenced by IDs to attach to the input.
--
-- * 'uiDestinations' - Destination settings for PUSH type inputs.
--
-- * 'uiName' - Name of the input.
--
-- * 'uiInputId' - Unique ID of the input.
updateInput
    :: Text -- ^ 'uiInputId'
    -> UpdateInput
updateInput pInputId_ =
  UpdateInput'
    { _uiSources = Nothing
    , _uiInputSecurityGroups = Nothing
    , _uiDestinations = Nothing
    , _uiName = Nothing
    , _uiInputId = pInputId_
    }


-- | The source URLs for a PULL-type input. Every PULL type input needs exactly two source URLs for redundancy. Only specify sources for PULL type Inputs. Leave Destinations empty.
uiSources :: Lens' UpdateInput [InputSourceRequest]
uiSources = lens _uiSources (\ s a -> s{_uiSources = a}) . _Default . _Coerce

-- | A list of security groups referenced by IDs to attach to the input.
uiInputSecurityGroups :: Lens' UpdateInput [Text]
uiInputSecurityGroups = lens _uiInputSecurityGroups (\ s a -> s{_uiInputSecurityGroups = a}) . _Default . _Coerce

-- | Destination settings for PUSH type inputs.
uiDestinations :: Lens' UpdateInput [InputDestinationRequest]
uiDestinations = lens _uiDestinations (\ s a -> s{_uiDestinations = a}) . _Default . _Coerce

-- | Name of the input.
uiName :: Lens' UpdateInput (Maybe Text)
uiName = lens _uiName (\ s a -> s{_uiName = a})

-- | Unique ID of the input.
uiInputId :: Lens' UpdateInput Text
uiInputId = lens _uiInputId (\ s a -> s{_uiInputId = a})

instance AWSRequest UpdateInput where
        type Rs UpdateInput = UpdateInputResponse
        request = putJSON mediaLive
        response
          = receiveJSON
              (\ s h x ->
                 UpdateInputResponse' <$>
                   (x .?> "input") <*> (pure (fromEnum s)))

instance Hashable UpdateInput where

instance NFData UpdateInput where

instance ToHeaders UpdateInput where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateInput where
        toJSON UpdateInput'{..}
          = object
              (catMaybes
                 [("sources" .=) <$> _uiSources,
                  ("inputSecurityGroups" .=) <$>
                    _uiInputSecurityGroups,
                  ("destinations" .=) <$> _uiDestinations,
                  ("name" .=) <$> _uiName])

instance ToPath UpdateInput where
        toPath UpdateInput'{..}
          = mconcat ["/prod/inputs/", toBS _uiInputId]

instance ToQuery UpdateInput where
        toQuery = const mempty

-- | Placeholder documentation for UpdateInputResponse
--
-- /See:/ 'updateInputResponse' smart constructor.
data UpdateInputResponse = UpdateInputResponse'
  { _uirsInput          :: !(Maybe Input)
  , _uirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateInputResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uirsInput' - Undocumented member.
--
-- * 'uirsResponseStatus' - -- | The response status code.
updateInputResponse
    :: Int -- ^ 'uirsResponseStatus'
    -> UpdateInputResponse
updateInputResponse pResponseStatus_ =
  UpdateInputResponse'
    {_uirsInput = Nothing, _uirsResponseStatus = pResponseStatus_}


-- | Undocumented member.
uirsInput :: Lens' UpdateInputResponse (Maybe Input)
uirsInput = lens _uirsInput (\ s a -> s{_uirsInput = a})

-- | -- | The response status code.
uirsResponseStatus :: Lens' UpdateInputResponse Int
uirsResponseStatus = lens _uirsResponseStatus (\ s a -> s{_uirsResponseStatus = a})

instance NFData UpdateInputResponse where
