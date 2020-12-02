{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.TreatmentResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.TreatmentResource where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.CampaignState
import Network.AWS.Pinpoint.Types.CustomDeliveryConfiguration
import Network.AWS.Pinpoint.Types.MessageConfiguration
import Network.AWS.Pinpoint.Types.Schedule
import Network.AWS.Pinpoint.Types.TemplateConfiguration
import Network.AWS.Prelude

-- | Specifies the settings for a campaign treatment. A /treatment/ is a variation of a campaign that's used for A/B testing of a campaign.
--
--
--
-- /See:/ 'treatmentResource' smart constructor.
data TreatmentResource = TreatmentResource'
  { _trCustomDeliveryConfiguration ::
      !(Maybe CustomDeliveryConfiguration),
    _trState :: !(Maybe CampaignState),
    _trSchedule :: !(Maybe Schedule),
    _trTemplateConfiguration ::
      !(Maybe TemplateConfiguration),
    _trTreatmentName :: !(Maybe Text),
    _trTreatmentDescription :: !(Maybe Text),
    _trMessageConfiguration ::
      !(Maybe MessageConfiguration),
    _trId :: !Text,
    _trSizePercent :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TreatmentResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trCustomDeliveryConfiguration' - The delivery configuration settings for sending the treatment through a custom channel. This object is required if the MessageConfiguration object for the treatment specifies a CustomMessage object.
--
-- * 'trState' - The current status of the treatment.
--
-- * 'trSchedule' - The schedule settings for the treatment.
--
-- * 'trTemplateConfiguration' - The message template to use for the treatment.
--
-- * 'trTreatmentName' - The custom name of the treatment.
--
-- * 'trTreatmentDescription' - The custom description of the treatment.
--
-- * 'trMessageConfiguration' - The message configuration settings for the treatment.
--
-- * 'trId' - The unique identifier for the treatment.
--
-- * 'trSizePercent' - The allocated percentage of users (segment members) that the treatment is sent to.
treatmentResource ::
  -- | 'trId'
  Text ->
  -- | 'trSizePercent'
  Int ->
  TreatmentResource
treatmentResource pId_ pSizePercent_ =
  TreatmentResource'
    { _trCustomDeliveryConfiguration = Nothing,
      _trState = Nothing,
      _trSchedule = Nothing,
      _trTemplateConfiguration = Nothing,
      _trTreatmentName = Nothing,
      _trTreatmentDescription = Nothing,
      _trMessageConfiguration = Nothing,
      _trId = pId_,
      _trSizePercent = pSizePercent_
    }

-- | The delivery configuration settings for sending the treatment through a custom channel. This object is required if the MessageConfiguration object for the treatment specifies a CustomMessage object.
trCustomDeliveryConfiguration :: Lens' TreatmentResource (Maybe CustomDeliveryConfiguration)
trCustomDeliveryConfiguration = lens _trCustomDeliveryConfiguration (\s a -> s {_trCustomDeliveryConfiguration = a})

-- | The current status of the treatment.
trState :: Lens' TreatmentResource (Maybe CampaignState)
trState = lens _trState (\s a -> s {_trState = a})

-- | The schedule settings for the treatment.
trSchedule :: Lens' TreatmentResource (Maybe Schedule)
trSchedule = lens _trSchedule (\s a -> s {_trSchedule = a})

-- | The message template to use for the treatment.
trTemplateConfiguration :: Lens' TreatmentResource (Maybe TemplateConfiguration)
trTemplateConfiguration = lens _trTemplateConfiguration (\s a -> s {_trTemplateConfiguration = a})

-- | The custom name of the treatment.
trTreatmentName :: Lens' TreatmentResource (Maybe Text)
trTreatmentName = lens _trTreatmentName (\s a -> s {_trTreatmentName = a})

-- | The custom description of the treatment.
trTreatmentDescription :: Lens' TreatmentResource (Maybe Text)
trTreatmentDescription = lens _trTreatmentDescription (\s a -> s {_trTreatmentDescription = a})

-- | The message configuration settings for the treatment.
trMessageConfiguration :: Lens' TreatmentResource (Maybe MessageConfiguration)
trMessageConfiguration = lens _trMessageConfiguration (\s a -> s {_trMessageConfiguration = a})

-- | The unique identifier for the treatment.
trId :: Lens' TreatmentResource Text
trId = lens _trId (\s a -> s {_trId = a})

-- | The allocated percentage of users (segment members) that the treatment is sent to.
trSizePercent :: Lens' TreatmentResource Int
trSizePercent = lens _trSizePercent (\s a -> s {_trSizePercent = a})

instance FromJSON TreatmentResource where
  parseJSON =
    withObject
      "TreatmentResource"
      ( \x ->
          TreatmentResource'
            <$> (x .:? "CustomDeliveryConfiguration")
            <*> (x .:? "State")
            <*> (x .:? "Schedule")
            <*> (x .:? "TemplateConfiguration")
            <*> (x .:? "TreatmentName")
            <*> (x .:? "TreatmentDescription")
            <*> (x .:? "MessageConfiguration")
            <*> (x .: "Id")
            <*> (x .: "SizePercent")
      )

instance Hashable TreatmentResource

instance NFData TreatmentResource
