{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.TreatmentResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.TreatmentResource where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.CampaignState
import Network.AWS.Pinpoint.Types.CustomDeliveryConfiguration
import Network.AWS.Pinpoint.Types.MessageConfiguration
import Network.AWS.Pinpoint.Types.Schedule
import Network.AWS.Pinpoint.Types.TemplateConfiguration
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the settings for a campaign treatment. A /treatment/ is a
-- variation of a campaign that\'s used for A\/B testing of a campaign.
--
-- /See:/ 'newTreatmentResource' smart constructor.
data TreatmentResource = TreatmentResource'
  { -- | The delivery configuration settings for sending the treatment through a
    -- custom channel. This object is required if the MessageConfiguration
    -- object for the treatment specifies a CustomMessage object.
    customDeliveryConfiguration :: Prelude.Maybe CustomDeliveryConfiguration,
    -- | The current status of the treatment.
    state :: Prelude.Maybe CampaignState,
    -- | The custom name of the treatment.
    treatmentName :: Prelude.Maybe Prelude.Text,
    -- | The message configuration settings for the treatment.
    messageConfiguration :: Prelude.Maybe MessageConfiguration,
    -- | The message template to use for the treatment.
    templateConfiguration :: Prelude.Maybe TemplateConfiguration,
    -- | The schedule settings for the treatment.
    schedule :: Prelude.Maybe Schedule,
    -- | The custom description of the treatment.
    treatmentDescription :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the treatment.
    id :: Prelude.Text,
    -- | The allocated percentage of users (segment members) that the treatment
    -- is sent to.
    sizePercent :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TreatmentResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customDeliveryConfiguration', 'treatmentResource_customDeliveryConfiguration' - The delivery configuration settings for sending the treatment through a
-- custom channel. This object is required if the MessageConfiguration
-- object for the treatment specifies a CustomMessage object.
--
-- 'state', 'treatmentResource_state' - The current status of the treatment.
--
-- 'treatmentName', 'treatmentResource_treatmentName' - The custom name of the treatment.
--
-- 'messageConfiguration', 'treatmentResource_messageConfiguration' - The message configuration settings for the treatment.
--
-- 'templateConfiguration', 'treatmentResource_templateConfiguration' - The message template to use for the treatment.
--
-- 'schedule', 'treatmentResource_schedule' - The schedule settings for the treatment.
--
-- 'treatmentDescription', 'treatmentResource_treatmentDescription' - The custom description of the treatment.
--
-- 'id', 'treatmentResource_id' - The unique identifier for the treatment.
--
-- 'sizePercent', 'treatmentResource_sizePercent' - The allocated percentage of users (segment members) that the treatment
-- is sent to.
newTreatmentResource ::
  -- | 'id'
  Prelude.Text ->
  -- | 'sizePercent'
  Prelude.Int ->
  TreatmentResource
newTreatmentResource pId_ pSizePercent_ =
  TreatmentResource'
    { customDeliveryConfiguration =
        Prelude.Nothing,
      state = Prelude.Nothing,
      treatmentName = Prelude.Nothing,
      messageConfiguration = Prelude.Nothing,
      templateConfiguration = Prelude.Nothing,
      schedule = Prelude.Nothing,
      treatmentDescription = Prelude.Nothing,
      id = pId_,
      sizePercent = pSizePercent_
    }

-- | The delivery configuration settings for sending the treatment through a
-- custom channel. This object is required if the MessageConfiguration
-- object for the treatment specifies a CustomMessage object.
treatmentResource_customDeliveryConfiguration :: Lens.Lens' TreatmentResource (Prelude.Maybe CustomDeliveryConfiguration)
treatmentResource_customDeliveryConfiguration = Lens.lens (\TreatmentResource' {customDeliveryConfiguration} -> customDeliveryConfiguration) (\s@TreatmentResource' {} a -> s {customDeliveryConfiguration = a} :: TreatmentResource)

-- | The current status of the treatment.
treatmentResource_state :: Lens.Lens' TreatmentResource (Prelude.Maybe CampaignState)
treatmentResource_state = Lens.lens (\TreatmentResource' {state} -> state) (\s@TreatmentResource' {} a -> s {state = a} :: TreatmentResource)

-- | The custom name of the treatment.
treatmentResource_treatmentName :: Lens.Lens' TreatmentResource (Prelude.Maybe Prelude.Text)
treatmentResource_treatmentName = Lens.lens (\TreatmentResource' {treatmentName} -> treatmentName) (\s@TreatmentResource' {} a -> s {treatmentName = a} :: TreatmentResource)

-- | The message configuration settings for the treatment.
treatmentResource_messageConfiguration :: Lens.Lens' TreatmentResource (Prelude.Maybe MessageConfiguration)
treatmentResource_messageConfiguration = Lens.lens (\TreatmentResource' {messageConfiguration} -> messageConfiguration) (\s@TreatmentResource' {} a -> s {messageConfiguration = a} :: TreatmentResource)

-- | The message template to use for the treatment.
treatmentResource_templateConfiguration :: Lens.Lens' TreatmentResource (Prelude.Maybe TemplateConfiguration)
treatmentResource_templateConfiguration = Lens.lens (\TreatmentResource' {templateConfiguration} -> templateConfiguration) (\s@TreatmentResource' {} a -> s {templateConfiguration = a} :: TreatmentResource)

-- | The schedule settings for the treatment.
treatmentResource_schedule :: Lens.Lens' TreatmentResource (Prelude.Maybe Schedule)
treatmentResource_schedule = Lens.lens (\TreatmentResource' {schedule} -> schedule) (\s@TreatmentResource' {} a -> s {schedule = a} :: TreatmentResource)

-- | The custom description of the treatment.
treatmentResource_treatmentDescription :: Lens.Lens' TreatmentResource (Prelude.Maybe Prelude.Text)
treatmentResource_treatmentDescription = Lens.lens (\TreatmentResource' {treatmentDescription} -> treatmentDescription) (\s@TreatmentResource' {} a -> s {treatmentDescription = a} :: TreatmentResource)

-- | The unique identifier for the treatment.
treatmentResource_id :: Lens.Lens' TreatmentResource Prelude.Text
treatmentResource_id = Lens.lens (\TreatmentResource' {id} -> id) (\s@TreatmentResource' {} a -> s {id = a} :: TreatmentResource)

-- | The allocated percentage of users (segment members) that the treatment
-- is sent to.
treatmentResource_sizePercent :: Lens.Lens' TreatmentResource Prelude.Int
treatmentResource_sizePercent = Lens.lens (\TreatmentResource' {sizePercent} -> sizePercent) (\s@TreatmentResource' {} a -> s {sizePercent = a} :: TreatmentResource)

instance Prelude.FromJSON TreatmentResource where
  parseJSON =
    Prelude.withObject
      "TreatmentResource"
      ( \x ->
          TreatmentResource'
            Prelude.<$> (x Prelude..:? "CustomDeliveryConfiguration")
            Prelude.<*> (x Prelude..:? "State")
            Prelude.<*> (x Prelude..:? "TreatmentName")
            Prelude.<*> (x Prelude..:? "MessageConfiguration")
            Prelude.<*> (x Prelude..:? "TemplateConfiguration")
            Prelude.<*> (x Prelude..:? "Schedule")
            Prelude.<*> (x Prelude..:? "TreatmentDescription")
            Prelude.<*> (x Prelude..: "Id")
            Prelude.<*> (x Prelude..: "SizePercent")
      )

instance Prelude.Hashable TreatmentResource

instance Prelude.NFData TreatmentResource
