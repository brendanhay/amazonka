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
-- Module      : Amazonka.SNS.Types.Endpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SNS.Types.Endpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The endpoint for mobile app and device.
--
-- /See:/ 'newEndpoint' smart constructor.
data Endpoint = Endpoint'
  { -- | Attributes for endpoint.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The @EndpointArn@ for mobile app and device.
    endpointArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Endpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'endpoint_attributes' - Attributes for endpoint.
--
-- 'endpointArn', 'endpoint_endpointArn' - The @EndpointArn@ for mobile app and device.
newEndpoint ::
  Endpoint
newEndpoint =
  Endpoint'
    { attributes = Prelude.Nothing,
      endpointArn = Prelude.Nothing
    }

-- | Attributes for endpoint.
endpoint_attributes :: Lens.Lens' Endpoint (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
endpoint_attributes = Lens.lens (\Endpoint' {attributes} -> attributes) (\s@Endpoint' {} a -> s {attributes = a} :: Endpoint) Prelude.. Lens.mapping Lens.coerced

-- | The @EndpointArn@ for mobile app and device.
endpoint_endpointArn :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_endpointArn = Lens.lens (\Endpoint' {endpointArn} -> endpointArn) (\s@Endpoint' {} a -> s {endpointArn = a} :: Endpoint)

instance Core.FromXML Endpoint where
  parseXML x =
    Endpoint'
      Prelude.<$> ( x Core..@? "Attributes" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLMap "entry" "key" "value")
                  )
      Prelude.<*> (x Core..@? "EndpointArn")

instance Prelude.Hashable Endpoint where
  hashWithSalt _salt Endpoint' {..} =
    _salt `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` endpointArn

instance Prelude.NFData Endpoint where
  rnf Endpoint' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf endpointArn
