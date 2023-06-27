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
-- Module      : Amazonka.Connect.Types.WisdomInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.WisdomInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about Amazon Connect Wisdom.
--
-- /See:/ 'newWisdomInfo' smart constructor.
data WisdomInfo = WisdomInfo'
  { -- | The Amazon Resource Name (ARN) of the Wisdom session.
    sessionArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WisdomInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionArn', 'wisdomInfo_sessionArn' - The Amazon Resource Name (ARN) of the Wisdom session.
newWisdomInfo ::
  WisdomInfo
newWisdomInfo =
  WisdomInfo' {sessionArn = Prelude.Nothing}

-- | The Amazon Resource Name (ARN) of the Wisdom session.
wisdomInfo_sessionArn :: Lens.Lens' WisdomInfo (Prelude.Maybe Prelude.Text)
wisdomInfo_sessionArn = Lens.lens (\WisdomInfo' {sessionArn} -> sessionArn) (\s@WisdomInfo' {} a -> s {sessionArn = a} :: WisdomInfo)

instance Data.FromJSON WisdomInfo where
  parseJSON =
    Data.withObject
      "WisdomInfo"
      ( \x ->
          WisdomInfo' Prelude.<$> (x Data..:? "SessionArn")
      )

instance Prelude.Hashable WisdomInfo where
  hashWithSalt _salt WisdomInfo' {..} =
    _salt `Prelude.hashWithSalt` sessionArn

instance Prelude.NFData WisdomInfo where
  rnf WisdomInfo' {..} = Prelude.rnf sessionArn
