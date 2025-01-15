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
-- Module      : Amazonka.IoTEvents.Types.Payload
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.Payload where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types.PayloadType
import qualified Amazonka.Prelude as Prelude

-- | Information needed to configure the payload.
--
-- By default, AWS IoT Events generates a standard payload in JSON for any
-- action. This action payload contains all attribute-value pairs that have
-- the information about the detector model instance and the event
-- triggered the action. To configure the action payload, you can use
-- @contentExpression@.
--
-- /See:/ 'newPayload' smart constructor.
data Payload = Payload'
  { -- | The content of the payload. You can use a string expression that
    -- includes quoted strings (@\'\<string>\'@), variables
    -- (@$variable.\<variable-name>@), input values
    -- (@$input.\<input-name>.\<path-to-datum>@), string concatenations, and
    -- quoted strings that contain @${}@ as the content. The recommended
    -- maximum size of a content expression is 1 KB.
    contentExpression :: Prelude.Text,
    -- | The value of the payload type can be either @STRING@ or @JSON@.
    type' :: PayloadType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Payload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentExpression', 'payload_contentExpression' - The content of the payload. You can use a string expression that
-- includes quoted strings (@\'\<string>\'@), variables
-- (@$variable.\<variable-name>@), input values
-- (@$input.\<input-name>.\<path-to-datum>@), string concatenations, and
-- quoted strings that contain @${}@ as the content. The recommended
-- maximum size of a content expression is 1 KB.
--
-- 'type'', 'payload_type' - The value of the payload type can be either @STRING@ or @JSON@.
newPayload ::
  -- | 'contentExpression'
  Prelude.Text ->
  -- | 'type''
  PayloadType ->
  Payload
newPayload pContentExpression_ pType_ =
  Payload'
    { contentExpression = pContentExpression_,
      type' = pType_
    }

-- | The content of the payload. You can use a string expression that
-- includes quoted strings (@\'\<string>\'@), variables
-- (@$variable.\<variable-name>@), input values
-- (@$input.\<input-name>.\<path-to-datum>@), string concatenations, and
-- quoted strings that contain @${}@ as the content. The recommended
-- maximum size of a content expression is 1 KB.
payload_contentExpression :: Lens.Lens' Payload Prelude.Text
payload_contentExpression = Lens.lens (\Payload' {contentExpression} -> contentExpression) (\s@Payload' {} a -> s {contentExpression = a} :: Payload)

-- | The value of the payload type can be either @STRING@ or @JSON@.
payload_type :: Lens.Lens' Payload PayloadType
payload_type = Lens.lens (\Payload' {type'} -> type') (\s@Payload' {} a -> s {type' = a} :: Payload)

instance Data.FromJSON Payload where
  parseJSON =
    Data.withObject
      "Payload"
      ( \x ->
          Payload'
            Prelude.<$> (x Data..: "contentExpression")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable Payload where
  hashWithSalt _salt Payload' {..} =
    _salt
      `Prelude.hashWithSalt` contentExpression
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Payload where
  rnf Payload' {..} =
    Prelude.rnf contentExpression `Prelude.seq`
      Prelude.rnf type'

instance Data.ToJSON Payload where
  toJSON Payload' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("contentExpression" Data..= contentExpression),
            Prelude.Just ("type" Data..= type')
          ]
      )
