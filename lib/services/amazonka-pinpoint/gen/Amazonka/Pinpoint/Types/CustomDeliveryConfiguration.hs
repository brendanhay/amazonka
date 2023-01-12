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
-- Module      : Amazonka.Pinpoint.Types.CustomDeliveryConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.CustomDeliveryConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.EndpointTypesElement
import qualified Amazonka.Prelude as Prelude

-- | Specifies the delivery configuration settings for sending a campaign or
-- campaign treatment through a custom channel. This object is required if
-- you use the CampaignCustomMessage object to define the message to send
-- for the campaign or campaign treatment.
--
-- /See:/ 'newCustomDeliveryConfiguration' smart constructor.
data CustomDeliveryConfiguration = CustomDeliveryConfiguration'
  { -- | The types of endpoints to send the campaign or treatment to. Each valid
    -- value maps to a type of channel that you can associate with an endpoint
    -- by using the ChannelType property of an endpoint.
    endpointTypes :: Prelude.Maybe [EndpointTypesElement],
    -- | The destination to send the campaign or treatment to. This value can be
    -- one of the following:
    --
    -- -   The name or Amazon Resource Name (ARN) of an AWS Lambda function to
    --     invoke to handle delivery of the campaign or treatment.
    --
    -- -   The URL for a web application or service that supports HTTPS and can
    --     receive the message. The URL has to be a full URL, including the
    --     HTTPS protocol.
    deliveryUri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomDeliveryConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointTypes', 'customDeliveryConfiguration_endpointTypes' - The types of endpoints to send the campaign or treatment to. Each valid
-- value maps to a type of channel that you can associate with an endpoint
-- by using the ChannelType property of an endpoint.
--
-- 'deliveryUri', 'customDeliveryConfiguration_deliveryUri' - The destination to send the campaign or treatment to. This value can be
-- one of the following:
--
-- -   The name or Amazon Resource Name (ARN) of an AWS Lambda function to
--     invoke to handle delivery of the campaign or treatment.
--
-- -   The URL for a web application or service that supports HTTPS and can
--     receive the message. The URL has to be a full URL, including the
--     HTTPS protocol.
newCustomDeliveryConfiguration ::
  -- | 'deliveryUri'
  Prelude.Text ->
  CustomDeliveryConfiguration
newCustomDeliveryConfiguration pDeliveryUri_ =
  CustomDeliveryConfiguration'
    { endpointTypes =
        Prelude.Nothing,
      deliveryUri = pDeliveryUri_
    }

-- | The types of endpoints to send the campaign or treatment to. Each valid
-- value maps to a type of channel that you can associate with an endpoint
-- by using the ChannelType property of an endpoint.
customDeliveryConfiguration_endpointTypes :: Lens.Lens' CustomDeliveryConfiguration (Prelude.Maybe [EndpointTypesElement])
customDeliveryConfiguration_endpointTypes = Lens.lens (\CustomDeliveryConfiguration' {endpointTypes} -> endpointTypes) (\s@CustomDeliveryConfiguration' {} a -> s {endpointTypes = a} :: CustomDeliveryConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The destination to send the campaign or treatment to. This value can be
-- one of the following:
--
-- -   The name or Amazon Resource Name (ARN) of an AWS Lambda function to
--     invoke to handle delivery of the campaign or treatment.
--
-- -   The URL for a web application or service that supports HTTPS and can
--     receive the message. The URL has to be a full URL, including the
--     HTTPS protocol.
customDeliveryConfiguration_deliveryUri :: Lens.Lens' CustomDeliveryConfiguration Prelude.Text
customDeliveryConfiguration_deliveryUri = Lens.lens (\CustomDeliveryConfiguration' {deliveryUri} -> deliveryUri) (\s@CustomDeliveryConfiguration' {} a -> s {deliveryUri = a} :: CustomDeliveryConfiguration)

instance Data.FromJSON CustomDeliveryConfiguration where
  parseJSON =
    Data.withObject
      "CustomDeliveryConfiguration"
      ( \x ->
          CustomDeliveryConfiguration'
            Prelude.<$> (x Data..:? "EndpointTypes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "DeliveryUri")
      )

instance Prelude.Hashable CustomDeliveryConfiguration where
  hashWithSalt _salt CustomDeliveryConfiguration' {..} =
    _salt `Prelude.hashWithSalt` endpointTypes
      `Prelude.hashWithSalt` deliveryUri

instance Prelude.NFData CustomDeliveryConfiguration where
  rnf CustomDeliveryConfiguration' {..} =
    Prelude.rnf endpointTypes
      `Prelude.seq` Prelude.rnf deliveryUri

instance Data.ToJSON CustomDeliveryConfiguration where
  toJSON CustomDeliveryConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EndpointTypes" Data..=) Prelude.<$> endpointTypes,
            Prelude.Just ("DeliveryUri" Data..= deliveryUri)
          ]
      )
