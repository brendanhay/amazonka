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
-- Module      : Amazonka.SecurityHub.Types.ClassificationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.ClassificationStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides details about the current status of the sensitive data
-- detection.
--
-- /See:/ 'newClassificationStatus' smart constructor.
data ClassificationStatus = ClassificationStatus'
  { -- | The code that represents the status of the sensitive data detection.
    code :: Prelude.Maybe Prelude.Text,
    -- | A longer description of the current status of the sensitive data
    -- detection.
    reason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClassificationStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'classificationStatus_code' - The code that represents the status of the sensitive data detection.
--
-- 'reason', 'classificationStatus_reason' - A longer description of the current status of the sensitive data
-- detection.
newClassificationStatus ::
  ClassificationStatus
newClassificationStatus =
  ClassificationStatus'
    { code = Prelude.Nothing,
      reason = Prelude.Nothing
    }

-- | The code that represents the status of the sensitive data detection.
classificationStatus_code :: Lens.Lens' ClassificationStatus (Prelude.Maybe Prelude.Text)
classificationStatus_code = Lens.lens (\ClassificationStatus' {code} -> code) (\s@ClassificationStatus' {} a -> s {code = a} :: ClassificationStatus)

-- | A longer description of the current status of the sensitive data
-- detection.
classificationStatus_reason :: Lens.Lens' ClassificationStatus (Prelude.Maybe Prelude.Text)
classificationStatus_reason = Lens.lens (\ClassificationStatus' {reason} -> reason) (\s@ClassificationStatus' {} a -> s {reason = a} :: ClassificationStatus)

instance Data.FromJSON ClassificationStatus where
  parseJSON =
    Data.withObject
      "ClassificationStatus"
      ( \x ->
          ClassificationStatus'
            Prelude.<$> (x Data..:? "Code")
            Prelude.<*> (x Data..:? "Reason")
      )

instance Prelude.Hashable ClassificationStatus where
  hashWithSalt _salt ClassificationStatus' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` reason

instance Prelude.NFData ClassificationStatus where
  rnf ClassificationStatus' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf reason

instance Data.ToJSON ClassificationStatus where
  toJSON ClassificationStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Code" Data..=) Prelude.<$> code,
            ("Reason" Data..=) Prelude.<$> reason
          ]
      )
