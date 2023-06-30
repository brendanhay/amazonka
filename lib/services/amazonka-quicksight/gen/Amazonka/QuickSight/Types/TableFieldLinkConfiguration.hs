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
-- Module      : Amazonka.QuickSight.Types.TableFieldLinkConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TableFieldLinkConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TableFieldLinkContentConfiguration
import Amazonka.QuickSight.Types.URLTargetConfiguration

-- | The link configuration of a table field URL.
--
-- /See:/ 'newTableFieldLinkConfiguration' smart constructor.
data TableFieldLinkConfiguration = TableFieldLinkConfiguration'
  { -- | The URL target (new tab, new window, same tab) for the table link
    -- configuration.
    target :: URLTargetConfiguration,
    -- | The URL content (text, icon) for the table link configuration.
    content :: TableFieldLinkContentConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableFieldLinkConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'target', 'tableFieldLinkConfiguration_target' - The URL target (new tab, new window, same tab) for the table link
-- configuration.
--
-- 'content', 'tableFieldLinkConfiguration_content' - The URL content (text, icon) for the table link configuration.
newTableFieldLinkConfiguration ::
  -- | 'target'
  URLTargetConfiguration ->
  -- | 'content'
  TableFieldLinkContentConfiguration ->
  TableFieldLinkConfiguration
newTableFieldLinkConfiguration pTarget_ pContent_ =
  TableFieldLinkConfiguration'
    { target = pTarget_,
      content = pContent_
    }

-- | The URL target (new tab, new window, same tab) for the table link
-- configuration.
tableFieldLinkConfiguration_target :: Lens.Lens' TableFieldLinkConfiguration URLTargetConfiguration
tableFieldLinkConfiguration_target = Lens.lens (\TableFieldLinkConfiguration' {target} -> target) (\s@TableFieldLinkConfiguration' {} a -> s {target = a} :: TableFieldLinkConfiguration)

-- | The URL content (text, icon) for the table link configuration.
tableFieldLinkConfiguration_content :: Lens.Lens' TableFieldLinkConfiguration TableFieldLinkContentConfiguration
tableFieldLinkConfiguration_content = Lens.lens (\TableFieldLinkConfiguration' {content} -> content) (\s@TableFieldLinkConfiguration' {} a -> s {content = a} :: TableFieldLinkConfiguration)

instance Data.FromJSON TableFieldLinkConfiguration where
  parseJSON =
    Data.withObject
      "TableFieldLinkConfiguration"
      ( \x ->
          TableFieldLinkConfiguration'
            Prelude.<$> (x Data..: "Target")
            Prelude.<*> (x Data..: "Content")
      )

instance Prelude.Hashable TableFieldLinkConfiguration where
  hashWithSalt _salt TableFieldLinkConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` target
      `Prelude.hashWithSalt` content

instance Prelude.NFData TableFieldLinkConfiguration where
  rnf TableFieldLinkConfiguration' {..} =
    Prelude.rnf target
      `Prelude.seq` Prelude.rnf content

instance Data.ToJSON TableFieldLinkConfiguration where
  toJSON TableFieldLinkConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Target" Data..= target),
            Prelude.Just ("Content" Data..= content)
          ]
      )
