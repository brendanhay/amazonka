{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.ParticipantDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.ParticipantDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The customer's details.
--
--
--
-- /See:/ 'participantDetails' smart constructor.
newtype ParticipantDetails = ParticipantDetails'
  { _pdDisplayName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ParticipantDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdDisplayName' - Display name of the participant.
participantDetails ::
  -- | 'pdDisplayName'
  Text ->
  ParticipantDetails
participantDetails pDisplayName_ =
  ParticipantDetails' {_pdDisplayName = pDisplayName_}

-- | Display name of the participant.
pdDisplayName :: Lens' ParticipantDetails Text
pdDisplayName = lens _pdDisplayName (\s a -> s {_pdDisplayName = a})

instance Hashable ParticipantDetails

instance NFData ParticipantDetails

instance ToJSON ParticipantDetails where
  toJSON ParticipantDetails' {..} =
    object (catMaybes [Just ("DisplayName" .= _pdDisplayName)])
