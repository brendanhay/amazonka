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
-- Module      : Amazonka.ElasticBeanstalk.Types.Builder
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.Builder where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The builder used to build the custom platform.
--
-- /See:/ 'newBuilder' smart constructor.
data Builder = Builder'
  { -- | The ARN of the builder.
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Builder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'builder_arn' - The ARN of the builder.
newBuilder ::
  Builder
newBuilder = Builder' {arn = Prelude.Nothing}

-- | The ARN of the builder.
builder_arn :: Lens.Lens' Builder (Prelude.Maybe Prelude.Text)
builder_arn = Lens.lens (\Builder' {arn} -> arn) (\s@Builder' {} a -> s {arn = a} :: Builder)

instance Data.FromXML Builder where
  parseXML x = Builder' Prelude.<$> (x Data..@? "ARN")

instance Prelude.Hashable Builder where
  hashWithSalt _salt Builder' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData Builder where
  rnf Builder' {..} = Prelude.rnf arn
