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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.CodeDeliveryDetailsType where

import Amazonka.CognitoIdentityProvider.Types.DeliveryMediumType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The code delivery details being returned from the server.
--
-- /See:/ 'newCodeDeliveryDetailsType' smart constructor.
data CodeDeliveryDetailsType = CodeDeliveryDetailsType'
  { -- | The destination for the code delivery details.
    destination :: Prelude.Maybe Prelude.Text,
    -- | The delivery medium (email message or phone number).
    deliveryMedium :: Prelude.Maybe DeliveryMediumType,
    -- | The attribute name.
    attributeName :: Prelude.Maybe Prelude.Text
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
-- 'destination', 'codeDeliveryDetailsType_destination' - The destination for the code delivery details.
--
-- 'deliveryMedium', 'codeDeliveryDetailsType_deliveryMedium' - The delivery medium (email message or phone number).
--
-- 'attributeName', 'codeDeliveryDetailsType_attributeName' - The attribute name.
newCodeDeliveryDetailsType ::
  CodeDeliveryDetailsType
newCodeDeliveryDetailsType =
  CodeDeliveryDetailsType'
    { destination =
        Prelude.Nothing,
      deliveryMedium = Prelude.Nothing,
      attributeName = Prelude.Nothing
    }

-- | The destination for the code delivery details.
codeDeliveryDetailsType_destination :: Lens.Lens' CodeDeliveryDetailsType (Prelude.Maybe Prelude.Text)
codeDeliveryDetailsType_destination = Lens.lens (\CodeDeliveryDetailsType' {destination} -> destination) (\s@CodeDeliveryDetailsType' {} a -> s {destination = a} :: CodeDeliveryDetailsType)

-- | The delivery medium (email message or phone number).
codeDeliveryDetailsType_deliveryMedium :: Lens.Lens' CodeDeliveryDetailsType (Prelude.Maybe DeliveryMediumType)
codeDeliveryDetailsType_deliveryMedium = Lens.lens (\CodeDeliveryDetailsType' {deliveryMedium} -> deliveryMedium) (\s@CodeDeliveryDetailsType' {} a -> s {deliveryMedium = a} :: CodeDeliveryDetailsType)

-- | The attribute name.
codeDeliveryDetailsType_attributeName :: Lens.Lens' CodeDeliveryDetailsType (Prelude.Maybe Prelude.Text)
codeDeliveryDetailsType_attributeName = Lens.lens (\CodeDeliveryDetailsType' {attributeName} -> attributeName) (\s@CodeDeliveryDetailsType' {} a -> s {attributeName = a} :: CodeDeliveryDetailsType)

instance Core.FromJSON CodeDeliveryDetailsType where
  parseJSON =
    Core.withObject
      "CodeDeliveryDetailsType"
      ( \x ->
          CodeDeliveryDetailsType'
            Prelude.<$> (x Core..:? "Destination")
            Prelude.<*> (x Core..:? "DeliveryMedium")
            Prelude.<*> (x Core..:? "AttributeName")
      )

instance Prelude.Hashable CodeDeliveryDetailsType

instance Prelude.NFData CodeDeliveryDetailsType
