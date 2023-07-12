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
-- Module      : Amazonka.CloudFront.Types.FunctionConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.FunctionConfig where

import Amazonka.CloudFront.Types.FunctionRuntime
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains configuration information about a CloudFront function.
--
-- /See:/ 'newFunctionConfig' smart constructor.
data FunctionConfig = FunctionConfig'
  { -- | A comment to describe the function.
    comment :: Prelude.Text,
    -- | The function\'s runtime environment. The only valid value is
    -- @cloudfront-js-1.0@.
    runtime :: FunctionRuntime
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FunctionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'functionConfig_comment' - A comment to describe the function.
--
-- 'runtime', 'functionConfig_runtime' - The function\'s runtime environment. The only valid value is
-- @cloudfront-js-1.0@.
newFunctionConfig ::
  -- | 'comment'
  Prelude.Text ->
  -- | 'runtime'
  FunctionRuntime ->
  FunctionConfig
newFunctionConfig pComment_ pRuntime_ =
  FunctionConfig'
    { comment = pComment_,
      runtime = pRuntime_
    }

-- | A comment to describe the function.
functionConfig_comment :: Lens.Lens' FunctionConfig Prelude.Text
functionConfig_comment = Lens.lens (\FunctionConfig' {comment} -> comment) (\s@FunctionConfig' {} a -> s {comment = a} :: FunctionConfig)

-- | The function\'s runtime environment. The only valid value is
-- @cloudfront-js-1.0@.
functionConfig_runtime :: Lens.Lens' FunctionConfig FunctionRuntime
functionConfig_runtime = Lens.lens (\FunctionConfig' {runtime} -> runtime) (\s@FunctionConfig' {} a -> s {runtime = a} :: FunctionConfig)

instance Data.FromXML FunctionConfig where
  parseXML x =
    FunctionConfig'
      Prelude.<$> (x Data..@ "Comment")
      Prelude.<*> (x Data..@ "Runtime")

instance Prelude.Hashable FunctionConfig where
  hashWithSalt _salt FunctionConfig' {..} =
    _salt
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` runtime

instance Prelude.NFData FunctionConfig where
  rnf FunctionConfig' {..} =
    Prelude.rnf comment
      `Prelude.seq` Prelude.rnf runtime

instance Data.ToXML FunctionConfig where
  toXML FunctionConfig' {..} =
    Prelude.mconcat
      [ "Comment" Data.@= comment,
        "Runtime" Data.@= runtime
      ]
