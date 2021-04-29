{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Connect.Types.QuickConnectSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.QuickConnectSummary where

import Network.AWS.Connect.Types.QuickConnectType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains summary information about a quick connect.
--
-- /See:/ 'newQuickConnectSummary' smart constructor.
data QuickConnectSummary = QuickConnectSummary'
  { -- | The type of quick connect. In the Amazon Connect console, when you
    -- create a quick connect, you are prompted to assign one of the following
    -- types: Agent (USER), External (PHONE_NUMBER), or Queue (QUEUE).
    quickConnectType :: Prelude.Maybe QuickConnectType,
    -- | The Amazon Resource Name (ARN) of the quick connect.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the quick connect.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the quick connect.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'QuickConnectSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'quickConnectType', 'quickConnectSummary_quickConnectType' - The type of quick connect. In the Amazon Connect console, when you
-- create a quick connect, you are prompted to assign one of the following
-- types: Agent (USER), External (PHONE_NUMBER), or Queue (QUEUE).
--
-- 'arn', 'quickConnectSummary_arn' - The Amazon Resource Name (ARN) of the quick connect.
--
-- 'id', 'quickConnectSummary_id' - The identifier for the quick connect.
--
-- 'name', 'quickConnectSummary_name' - The name of the quick connect.
newQuickConnectSummary ::
  QuickConnectSummary
newQuickConnectSummary =
  QuickConnectSummary'
    { quickConnectType =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The type of quick connect. In the Amazon Connect console, when you
-- create a quick connect, you are prompted to assign one of the following
-- types: Agent (USER), External (PHONE_NUMBER), or Queue (QUEUE).
quickConnectSummary_quickConnectType :: Lens.Lens' QuickConnectSummary (Prelude.Maybe QuickConnectType)
quickConnectSummary_quickConnectType = Lens.lens (\QuickConnectSummary' {quickConnectType} -> quickConnectType) (\s@QuickConnectSummary' {} a -> s {quickConnectType = a} :: QuickConnectSummary)

-- | The Amazon Resource Name (ARN) of the quick connect.
quickConnectSummary_arn :: Lens.Lens' QuickConnectSummary (Prelude.Maybe Prelude.Text)
quickConnectSummary_arn = Lens.lens (\QuickConnectSummary' {arn} -> arn) (\s@QuickConnectSummary' {} a -> s {arn = a} :: QuickConnectSummary)

-- | The identifier for the quick connect.
quickConnectSummary_id :: Lens.Lens' QuickConnectSummary (Prelude.Maybe Prelude.Text)
quickConnectSummary_id = Lens.lens (\QuickConnectSummary' {id} -> id) (\s@QuickConnectSummary' {} a -> s {id = a} :: QuickConnectSummary)

-- | The name of the quick connect.
quickConnectSummary_name :: Lens.Lens' QuickConnectSummary (Prelude.Maybe Prelude.Text)
quickConnectSummary_name = Lens.lens (\QuickConnectSummary' {name} -> name) (\s@QuickConnectSummary' {} a -> s {name = a} :: QuickConnectSummary)

instance Prelude.FromJSON QuickConnectSummary where
  parseJSON =
    Prelude.withObject
      "QuickConnectSummary"
      ( \x ->
          QuickConnectSummary'
            Prelude.<$> (x Prelude..:? "QuickConnectType")
            Prelude.<*> (x Prelude..:? "Arn")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "Name")
      )

instance Prelude.Hashable QuickConnectSummary

instance Prelude.NFData QuickConnectSummary
