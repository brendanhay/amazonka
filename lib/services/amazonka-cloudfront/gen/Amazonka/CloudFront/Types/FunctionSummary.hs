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
-- Module      : Amazonka.CloudFront.Types.FunctionSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.FunctionSummary where

import Amazonka.CloudFront.Types.FunctionConfig
import Amazonka.CloudFront.Types.FunctionMetadata
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains configuration information and metadata about a CloudFront
-- function.
--
-- /See:/ 'newFunctionSummary' smart constructor.
data FunctionSummary = FunctionSummary'
  { -- | The status of the CloudFront function.
    status :: Prelude.Maybe Prelude.Text,
    -- | The name of the CloudFront function.
    name :: Prelude.Text,
    -- | Contains configuration information about a CloudFront function.
    functionConfig :: FunctionConfig,
    -- | Contains metadata about a CloudFront function.
    functionMetadata :: FunctionMetadata
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FunctionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'functionSummary_status' - The status of the CloudFront function.
--
-- 'name', 'functionSummary_name' - The name of the CloudFront function.
--
-- 'functionConfig', 'functionSummary_functionConfig' - Contains configuration information about a CloudFront function.
--
-- 'functionMetadata', 'functionSummary_functionMetadata' - Contains metadata about a CloudFront function.
newFunctionSummary ::
  -- | 'name'
  Prelude.Text ->
  -- | 'functionConfig'
  FunctionConfig ->
  -- | 'functionMetadata'
  FunctionMetadata ->
  FunctionSummary
newFunctionSummary
  pName_
  pFunctionConfig_
  pFunctionMetadata_ =
    FunctionSummary'
      { status = Prelude.Nothing,
        name = pName_,
        functionConfig = pFunctionConfig_,
        functionMetadata = pFunctionMetadata_
      }

-- | The status of the CloudFront function.
functionSummary_status :: Lens.Lens' FunctionSummary (Prelude.Maybe Prelude.Text)
functionSummary_status = Lens.lens (\FunctionSummary' {status} -> status) (\s@FunctionSummary' {} a -> s {status = a} :: FunctionSummary)

-- | The name of the CloudFront function.
functionSummary_name :: Lens.Lens' FunctionSummary Prelude.Text
functionSummary_name = Lens.lens (\FunctionSummary' {name} -> name) (\s@FunctionSummary' {} a -> s {name = a} :: FunctionSummary)

-- | Contains configuration information about a CloudFront function.
functionSummary_functionConfig :: Lens.Lens' FunctionSummary FunctionConfig
functionSummary_functionConfig = Lens.lens (\FunctionSummary' {functionConfig} -> functionConfig) (\s@FunctionSummary' {} a -> s {functionConfig = a} :: FunctionSummary)

-- | Contains metadata about a CloudFront function.
functionSummary_functionMetadata :: Lens.Lens' FunctionSummary FunctionMetadata
functionSummary_functionMetadata = Lens.lens (\FunctionSummary' {functionMetadata} -> functionMetadata) (\s@FunctionSummary' {} a -> s {functionMetadata = a} :: FunctionSummary)

instance Core.FromXML FunctionSummary where
  parseXML x =
    FunctionSummary'
      Prelude.<$> (x Core..@? "Status")
      Prelude.<*> (x Core..@ "Name")
      Prelude.<*> (x Core..@ "FunctionConfig")
      Prelude.<*> (x Core..@ "FunctionMetadata")

instance Prelude.Hashable FunctionSummary where
  hashWithSalt _salt FunctionSummary' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` functionConfig
      `Prelude.hashWithSalt` functionMetadata

instance Prelude.NFData FunctionSummary where
  rnf FunctionSummary' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf functionConfig
      `Prelude.seq` Prelude.rnf functionMetadata
