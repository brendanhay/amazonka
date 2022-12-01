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
-- Module      : Amazonka.CloudDirectory.Types.BatchAttachTypedLinkResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchAttachTypedLinkResponse where

import Amazonka.CloudDirectory.Types.TypedLinkSpecifier
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of a AttachTypedLink response operation.
--
-- /See:/ 'newBatchAttachTypedLinkResponse' smart constructor.
data BatchAttachTypedLinkResponse = BatchAttachTypedLinkResponse'
  { -- | Returns a typed link specifier as output.
    typedLinkSpecifier :: Prelude.Maybe TypedLinkSpecifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchAttachTypedLinkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typedLinkSpecifier', 'batchAttachTypedLinkResponse_typedLinkSpecifier' - Returns a typed link specifier as output.
newBatchAttachTypedLinkResponse ::
  BatchAttachTypedLinkResponse
newBatchAttachTypedLinkResponse =
  BatchAttachTypedLinkResponse'
    { typedLinkSpecifier =
        Prelude.Nothing
    }

-- | Returns a typed link specifier as output.
batchAttachTypedLinkResponse_typedLinkSpecifier :: Lens.Lens' BatchAttachTypedLinkResponse (Prelude.Maybe TypedLinkSpecifier)
batchAttachTypedLinkResponse_typedLinkSpecifier = Lens.lens (\BatchAttachTypedLinkResponse' {typedLinkSpecifier} -> typedLinkSpecifier) (\s@BatchAttachTypedLinkResponse' {} a -> s {typedLinkSpecifier = a} :: BatchAttachTypedLinkResponse)

instance Core.FromJSON BatchAttachTypedLinkResponse where
  parseJSON =
    Core.withObject
      "BatchAttachTypedLinkResponse"
      ( \x ->
          BatchAttachTypedLinkResponse'
            Prelude.<$> (x Core..:? "TypedLinkSpecifier")
      )

instance
  Prelude.Hashable
    BatchAttachTypedLinkResponse
  where
  hashWithSalt _salt BatchAttachTypedLinkResponse' {..} =
    _salt `Prelude.hashWithSalt` typedLinkSpecifier

instance Prelude.NFData BatchAttachTypedLinkResponse where
  rnf BatchAttachTypedLinkResponse' {..} =
    Prelude.rnf typedLinkSpecifier
