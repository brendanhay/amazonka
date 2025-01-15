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
-- Module      : Amazonka.CognitoIdentityProvider.Types.CodeDeliveryDetailsType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.CodeDeliveryDetailsType where

import Amazonka.CognitoIdentityProvider.Types.DeliveryMediumType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The delivery details for an email or SMS message that Amazon Cognito
-- sent for authentication or verification.
--
-- /See:/ 'newCodeDeliveryDetailsType' smart constructor.
data CodeDeliveryDetailsType = CodeDeliveryDetailsType'
  { -- | The name of the attribute that Amazon Cognito verifies with the code.
    attributeName :: Prelude.Maybe Prelude.Text,
    -- | The method that Amazon Cognito used to send the code.
    deliveryMedium :: Prelude.Maybe DeliveryMediumType,
    -- | The email address or phone number destination where Amazon Cognito sent
    -- the code.
    destination :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodeDeliveryDetailsType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeName', 'codeDeliveryDetailsType_attributeName' - The name of the attribute that Amazon Cognito verifies with the code.
--
-- 'deliveryMedium', 'codeDeliveryDetailsType_deliveryMedium' - The method that Amazon Cognito used to send the code.
--
-- 'destination', 'codeDeliveryDetailsType_destination' - The email address or phone number destination where Amazon Cognito sent
-- the code.
newCodeDeliveryDetailsType ::
  CodeDeliveryDetailsType
newCodeDeliveryDetailsType =
  CodeDeliveryDetailsType'
    { attributeName =
        Prelude.Nothing,
      deliveryMedium = Prelude.Nothing,
      destination = Prelude.Nothing
    }

-- | The name of the attribute that Amazon Cognito verifies with the code.
codeDeliveryDetailsType_attributeName :: Lens.Lens' CodeDeliveryDetailsType (Prelude.Maybe Prelude.Text)
codeDeliveryDetailsType_attributeName = Lens.lens (\CodeDeliveryDetailsType' {attributeName} -> attributeName) (\s@CodeDeliveryDetailsType' {} a -> s {attributeName = a} :: CodeDeliveryDetailsType)

-- | The method that Amazon Cognito used to send the code.
codeDeliveryDetailsType_deliveryMedium :: Lens.Lens' CodeDeliveryDetailsType (Prelude.Maybe DeliveryMediumType)
codeDeliveryDetailsType_deliveryMedium = Lens.lens (\CodeDeliveryDetailsType' {deliveryMedium} -> deliveryMedium) (\s@CodeDeliveryDetailsType' {} a -> s {deliveryMedium = a} :: CodeDeliveryDetailsType)

-- | The email address or phone number destination where Amazon Cognito sent
-- the code.
codeDeliveryDetailsType_destination :: Lens.Lens' CodeDeliveryDetailsType (Prelude.Maybe Prelude.Text)
codeDeliveryDetailsType_destination = Lens.lens (\CodeDeliveryDetailsType' {destination} -> destination) (\s@CodeDeliveryDetailsType' {} a -> s {destination = a} :: CodeDeliveryDetailsType)

instance Data.FromJSON CodeDeliveryDetailsType where
  parseJSON =
    Data.withObject
      "CodeDeliveryDetailsType"
      ( \x ->
          CodeDeliveryDetailsType'
            Prelude.<$> (x Data..:? "AttributeName")
            Prelude.<*> (x Data..:? "DeliveryMedium")
            Prelude.<*> (x Data..:? "Destination")
      )

instance Prelude.Hashable CodeDeliveryDetailsType where
  hashWithSalt _salt CodeDeliveryDetailsType' {..} =
    _salt
      `Prelude.hashWithSalt` attributeName
      `Prelude.hashWithSalt` deliveryMedium
      `Prelude.hashWithSalt` destination

instance Prelude.NFData CodeDeliveryDetailsType where
  rnf CodeDeliveryDetailsType' {..} =
    Prelude.rnf attributeName `Prelude.seq`
      Prelude.rnf deliveryMedium `Prelude.seq`
        Prelude.rnf destination
