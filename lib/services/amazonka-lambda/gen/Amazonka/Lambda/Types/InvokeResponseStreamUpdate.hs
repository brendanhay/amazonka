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
-- Module      : Amazonka.Lambda.Types.InvokeResponseStreamUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.InvokeResponseStreamUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A chunk of the streamed response payload.
--
-- /See:/ 'newInvokeResponseStreamUpdate' smart constructor.
data InvokeResponseStreamUpdate = InvokeResponseStreamUpdate'
  { -- | Data returned by your Lambda function.
    payload :: Prelude.Maybe (Data.Sensitive Data.Base64)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InvokeResponseStreamUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'payload', 'invokeResponseStreamUpdate_payload' - Data returned by your Lambda function.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newInvokeResponseStreamUpdate ::
  InvokeResponseStreamUpdate
newInvokeResponseStreamUpdate =
  InvokeResponseStreamUpdate'
    { payload =
        Prelude.Nothing
    }

-- | Data returned by your Lambda function.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
invokeResponseStreamUpdate_payload :: Lens.Lens' InvokeResponseStreamUpdate (Prelude.Maybe Prelude.ByteString)
invokeResponseStreamUpdate_payload = Lens.lens (\InvokeResponseStreamUpdate' {payload} -> payload) (\s@InvokeResponseStreamUpdate' {} a -> s {payload = a} :: InvokeResponseStreamUpdate) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Data._Base64)

instance Data.FromJSON InvokeResponseStreamUpdate where
  parseJSON =
    Data.withObject
      "InvokeResponseStreamUpdate"
      ( \x ->
          InvokeResponseStreamUpdate'
            Prelude.<$> (x Data..:? "Payload")
      )

instance Prelude.Hashable InvokeResponseStreamUpdate where
  hashWithSalt _salt InvokeResponseStreamUpdate' {..} =
    _salt `Prelude.hashWithSalt` payload

instance Prelude.NFData InvokeResponseStreamUpdate where
  rnf InvokeResponseStreamUpdate' {..} =
    Prelude.rnf payload
