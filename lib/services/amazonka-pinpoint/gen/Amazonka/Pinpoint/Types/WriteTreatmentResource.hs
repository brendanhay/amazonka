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
-- Module      : Amazonka.Pinpoint.Types.WriteTreatmentResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.WriteTreatmentResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.CustomDeliveryConfiguration
import Amazonka.Pinpoint.Types.MessageConfiguration
import Amazonka.Pinpoint.Types.Schedule
import Amazonka.Pinpoint.Types.TemplateConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Specifies the settings for a campaign treatment. A /treatment/ is a
-- variation of a campaign that\'s used for A\/B testing of a campaign.
--
-- /See:/ 'newWriteTreatmentResource' smart constructor.
data WriteTreatmentResource = WriteTreatmentResource'
  { -- | The delivery configuration settings for sending the treatment through a
    -- custom channel. This object is required if the MessageConfiguration
    -- object for the treatment specifies a CustomMessage object.
    customDeliveryConfiguration :: Prelude.Maybe CustomDeliveryConfiguration,
    -- | The message configuration settings for the treatment.
    messageConfiguration :: Prelude.Maybe MessageConfiguration,
    -- | The schedule settings for the treatment.
    schedule :: Prelude.Maybe Schedule,
    -- | The message template to use for the treatment.
    templateConfiguration :: Prelude.Maybe TemplateConfiguration,
    -- | A custom description of the treatment.
    treatmentDescription :: Prelude.Maybe Prelude.Text,
    -- | A custom name for the treatment.
    treatmentName :: Prelude.Maybe Prelude.Text,
    -- | The allocated percentage of users (segment members) to send the
    -- treatment to.
    sizePercent :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WriteTreatmentResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customDeliveryConfiguration', 'writeTreatmentResource_customDeliveryConfiguration' - The delivery configuration settings for sending the treatment through a
-- custom channel. This object is required if the MessageConfiguration
-- object for the treatment specifies a CustomMessage object.
--
-- 'messageConfiguration', 'writeTreatmentResource_messageConfiguration' - The message configuration settings for the treatment.
--
-- 'schedule', 'writeTreatmentResource_schedule' - The schedule settings for the treatment.
--
-- 'templateConfiguration', 'writeTreatmentResource_templateConfiguration' - The message template to use for the treatment.
--
-- 'treatmentDescription', 'writeTreatmentResource_treatmentDescription' - A custom description of the treatment.
--
-- 'treatmentName', 'writeTreatmentResource_treatmentName' - A custom name for the treatment.
--
-- 'sizePercent', 'writeTreatmentResource_sizePercent' - The allocated percentage of users (segment members) to send the
-- treatment to.
newWriteTreatmentResource ::
  -- | 'sizePercent'
  Prelude.Int ->
  WriteTreatmentResource
newWriteTreatmentResource pSizePercent_ =
  WriteTreatmentResource'
    { customDeliveryConfiguration =
        Prelude.Nothing,
      messageConfiguration = Prelude.Nothing,
      schedule = Prelude.Nothing,
      templateConfiguration = Prelude.Nothing,
      treatmentDescription = Prelude.Nothing,
      treatmentName = Prelude.Nothing,
      sizePercent = pSizePercent_
    }

-- | The delivery configuration settings for sending the treatment through a
-- custom channel. This object is required if the MessageConfiguration
-- object for the treatment specifies a CustomMessage object.
writeTreatmentResource_customDeliveryConfiguration :: Lens.Lens' WriteTreatmentResource (Prelude.Maybe CustomDeliveryConfiguration)
writeTreatmentResource_customDeliveryConfiguration = Lens.lens (\WriteTreatmentResource' {customDeliveryConfiguration} -> customDeliveryConfiguration) (\s@WriteTreatmentResource' {} a -> s {customDeliveryConfiguration = a} :: WriteTreatmentResource)

-- | The message configuration settings for the treatment.
writeTreatmentResource_messageConfiguration :: Lens.Lens' WriteTreatmentResource (Prelude.Maybe MessageConfiguration)
writeTreatmentResource_messageConfiguration = Lens.lens (\WriteTreatmentResource' {messageConfiguration} -> messageConfiguration) (\s@WriteTreatmentResource' {} a -> s {messageConfiguration = a} :: WriteTreatmentResource)

-- | The schedule settings for the treatment.
writeTreatmentResource_schedule :: Lens.Lens' WriteTreatmentResource (Prelude.Maybe Schedule)
writeTreatmentResource_schedule = Lens.lens (\WriteTreatmentResource' {schedule} -> schedule) (\s@WriteTreatmentResource' {} a -> s {schedule = a} :: WriteTreatmentResource)

-- | The message template to use for the treatment.
writeTreatmentResource_templateConfiguration :: Lens.Lens' WriteTreatmentResource (Prelude.Maybe TemplateConfiguration)
writeTreatmentResource_templateConfiguration = Lens.lens (\WriteTreatmentResource' {templateConfiguration} -> templateConfiguration) (\s@WriteTreatmentResource' {} a -> s {templateConfiguration = a} :: WriteTreatmentResource)

-- | A custom description of the treatment.
writeTreatmentResource_treatmentDescription :: Lens.Lens' WriteTreatmentResource (Prelude.Maybe Prelude.Text)
writeTreatmentResource_treatmentDescription = Lens.lens (\WriteTreatmentResource' {treatmentDescription} -> treatmentDescription) (\s@WriteTreatmentResource' {} a -> s {treatmentDescription = a} :: WriteTreatmentResource)

-- | A custom name for the treatment.
writeTreatmentResource_treatmentName :: Lens.Lens' WriteTreatmentResource (Prelude.Maybe Prelude.Text)
writeTreatmentResource_treatmentName = Lens.lens (\WriteTreatmentResource' {treatmentName} -> treatmentName) (\s@WriteTreatmentResource' {} a -> s {treatmentName = a} :: WriteTreatmentResource)

-- | The allocated percentage of users (segment members) to send the
-- treatment to.
writeTreatmentResource_sizePercent :: Lens.Lens' WriteTreatmentResource Prelude.Int
writeTreatmentResource_sizePercent = Lens.lens (\WriteTreatmentResource' {sizePercent} -> sizePercent) (\s@WriteTreatmentResource' {} a -> s {sizePercent = a} :: WriteTreatmentResource)

instance Prelude.Hashable WriteTreatmentResource where
  hashWithSalt _salt WriteTreatmentResource' {..} =
    _salt
      `Prelude.hashWithSalt` customDeliveryConfiguration
      `Prelude.hashWithSalt` messageConfiguration
      `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` templateConfiguration
      `Prelude.hashWithSalt` treatmentDescription
      `Prelude.hashWithSalt` treatmentName
      `Prelude.hashWithSalt` sizePercent

instance Prelude.NFData WriteTreatmentResource where
  rnf WriteTreatmentResource' {..} =
    Prelude.rnf customDeliveryConfiguration `Prelude.seq`
      Prelude.rnf messageConfiguration `Prelude.seq`
        Prelude.rnf schedule `Prelude.seq`
          Prelude.rnf templateConfiguration `Prelude.seq`
            Prelude.rnf treatmentDescription `Prelude.seq`
              Prelude.rnf treatmentName `Prelude.seq`
                Prelude.rnf sizePercent

instance Data.ToJSON WriteTreatmentResource where
  toJSON WriteTreatmentResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomDeliveryConfiguration" Data..=)
              Prelude.<$> customDeliveryConfiguration,
            ("MessageConfiguration" Data..=)
              Prelude.<$> messageConfiguration,
            ("Schedule" Data..=) Prelude.<$> schedule,
            ("TemplateConfiguration" Data..=)
              Prelude.<$> templateConfiguration,
            ("TreatmentDescription" Data..=)
              Prelude.<$> treatmentDescription,
            ("TreatmentName" Data..=) Prelude.<$> treatmentName,
            Prelude.Just ("SizePercent" Data..= sizePercent)
          ]
      )
