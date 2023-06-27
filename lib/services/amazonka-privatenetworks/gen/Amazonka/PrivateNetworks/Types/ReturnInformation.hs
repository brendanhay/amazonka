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
-- Module      : Amazonka.PrivateNetworks.Types.ReturnInformation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PrivateNetworks.Types.ReturnInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types.Address

-- | Information about a request to return a network resource.
--
-- /See:/ 'newReturnInformation' smart constructor.
data ReturnInformation = ReturnInformation'
  { -- | The Amazon Resource Name (ARN) of the replacement order.
    replacementOrderArn :: Prelude.Maybe Prelude.Text,
    -- | The reason for the return. If the return request did not include a
    -- reason for the return, this value is null.
    returnReason :: Prelude.Maybe Prelude.Text,
    -- | The shipping address.
    shippingAddress :: Prelude.Maybe Address,
    -- | The URL of the shipping label. The shipping label is available for
    -- download only if the status of the network resource is @PENDING_RETURN@.
    -- For more information, see
    -- <https://docs.aws.amazon.com/private-networks/latest/userguide/radio-units.html#return-radio-unit Return a radio unit>.
    shippingLabel :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReturnInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replacementOrderArn', 'returnInformation_replacementOrderArn' - The Amazon Resource Name (ARN) of the replacement order.
--
-- 'returnReason', 'returnInformation_returnReason' - The reason for the return. If the return request did not include a
-- reason for the return, this value is null.
--
-- 'shippingAddress', 'returnInformation_shippingAddress' - The shipping address.
--
-- 'shippingLabel', 'returnInformation_shippingLabel' - The URL of the shipping label. The shipping label is available for
-- download only if the status of the network resource is @PENDING_RETURN@.
-- For more information, see
-- <https://docs.aws.amazon.com/private-networks/latest/userguide/radio-units.html#return-radio-unit Return a radio unit>.
newReturnInformation ::
  ReturnInformation
newReturnInformation =
  ReturnInformation'
    { replacementOrderArn =
        Prelude.Nothing,
      returnReason = Prelude.Nothing,
      shippingAddress = Prelude.Nothing,
      shippingLabel = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the replacement order.
returnInformation_replacementOrderArn :: Lens.Lens' ReturnInformation (Prelude.Maybe Prelude.Text)
returnInformation_replacementOrderArn = Lens.lens (\ReturnInformation' {replacementOrderArn} -> replacementOrderArn) (\s@ReturnInformation' {} a -> s {replacementOrderArn = a} :: ReturnInformation)

-- | The reason for the return. If the return request did not include a
-- reason for the return, this value is null.
returnInformation_returnReason :: Lens.Lens' ReturnInformation (Prelude.Maybe Prelude.Text)
returnInformation_returnReason = Lens.lens (\ReturnInformation' {returnReason} -> returnReason) (\s@ReturnInformation' {} a -> s {returnReason = a} :: ReturnInformation)

-- | The shipping address.
returnInformation_shippingAddress :: Lens.Lens' ReturnInformation (Prelude.Maybe Address)
returnInformation_shippingAddress = Lens.lens (\ReturnInformation' {shippingAddress} -> shippingAddress) (\s@ReturnInformation' {} a -> s {shippingAddress = a} :: ReturnInformation)

-- | The URL of the shipping label. The shipping label is available for
-- download only if the status of the network resource is @PENDING_RETURN@.
-- For more information, see
-- <https://docs.aws.amazon.com/private-networks/latest/userguide/radio-units.html#return-radio-unit Return a radio unit>.
returnInformation_shippingLabel :: Lens.Lens' ReturnInformation (Prelude.Maybe Prelude.Text)
returnInformation_shippingLabel = Lens.lens (\ReturnInformation' {shippingLabel} -> shippingLabel) (\s@ReturnInformation' {} a -> s {shippingLabel = a} :: ReturnInformation)

instance Data.FromJSON ReturnInformation where
  parseJSON =
    Data.withObject
      "ReturnInformation"
      ( \x ->
          ReturnInformation'
            Prelude.<$> (x Data..:? "replacementOrderArn")
            Prelude.<*> (x Data..:? "returnReason")
            Prelude.<*> (x Data..:? "shippingAddress")
            Prelude.<*> (x Data..:? "shippingLabel")
      )

instance Prelude.Hashable ReturnInformation where
  hashWithSalt _salt ReturnInformation' {..} =
    _salt
      `Prelude.hashWithSalt` replacementOrderArn
      `Prelude.hashWithSalt` returnReason
      `Prelude.hashWithSalt` shippingAddress
      `Prelude.hashWithSalt` shippingLabel

instance Prelude.NFData ReturnInformation where
  rnf ReturnInformation' {..} =
    Prelude.rnf replacementOrderArn
      `Prelude.seq` Prelude.rnf returnReason
      `Prelude.seq` Prelude.rnf shippingAddress
      `Prelude.seq` Prelude.rnf shippingLabel
