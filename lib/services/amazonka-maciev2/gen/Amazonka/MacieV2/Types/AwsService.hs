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
-- Module      : Amazonka.MacieV2.Types.AwsService
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.AwsService where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an Amazon Web Service that performed an
-- action on an affected resource.
--
-- /See:/ 'newAwsService' smart constructor.
data AwsService = AwsService'
  { -- | The name of the Amazon Web Service that performed the action.
    invokedBy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invokedBy', 'awsService_invokedBy' - The name of the Amazon Web Service that performed the action.
newAwsService ::
  AwsService
newAwsService =
  AwsService' {invokedBy = Prelude.Nothing}

-- | The name of the Amazon Web Service that performed the action.
awsService_invokedBy :: Lens.Lens' AwsService (Prelude.Maybe Prelude.Text)
awsService_invokedBy = Lens.lens (\AwsService' {invokedBy} -> invokedBy) (\s@AwsService' {} a -> s {invokedBy = a} :: AwsService)

instance Data.FromJSON AwsService where
  parseJSON =
    Data.withObject
      "AwsService"
      ( \x ->
          AwsService' Prelude.<$> (x Data..:? "invokedBy")
      )

instance Prelude.Hashable AwsService where
  hashWithSalt _salt AwsService' {..} =
    _salt `Prelude.hashWithSalt` invokedBy

instance Prelude.NFData AwsService where
  rnf AwsService' {..} = Prelude.rnf invokedBy
