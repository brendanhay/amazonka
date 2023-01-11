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
-- Module      : Amazonka.IoT.Types.AuthInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AuthInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.ActionType
import qualified Amazonka.Prelude as Prelude

-- | A collection of authorization information.
--
-- /See:/ 'newAuthInfo' smart constructor.
data AuthInfo = AuthInfo'
  { -- | The type of action for which the principal is being authorized.
    actionType :: Prelude.Maybe ActionType,
    -- | The resources for which the principal is being authorized to perform the
    -- specified action.
    resources :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionType', 'authInfo_actionType' - The type of action for which the principal is being authorized.
--
-- 'resources', 'authInfo_resources' - The resources for which the principal is being authorized to perform the
-- specified action.
newAuthInfo ::
  AuthInfo
newAuthInfo =
  AuthInfo'
    { actionType = Prelude.Nothing,
      resources = Prelude.mempty
    }

-- | The type of action for which the principal is being authorized.
authInfo_actionType :: Lens.Lens' AuthInfo (Prelude.Maybe ActionType)
authInfo_actionType = Lens.lens (\AuthInfo' {actionType} -> actionType) (\s@AuthInfo' {} a -> s {actionType = a} :: AuthInfo)

-- | The resources for which the principal is being authorized to perform the
-- specified action.
authInfo_resources :: Lens.Lens' AuthInfo [Prelude.Text]
authInfo_resources = Lens.lens (\AuthInfo' {resources} -> resources) (\s@AuthInfo' {} a -> s {resources = a} :: AuthInfo) Prelude.. Lens.coerced

instance Data.FromJSON AuthInfo where
  parseJSON =
    Data.withObject
      "AuthInfo"
      ( \x ->
          AuthInfo'
            Prelude.<$> (x Data..:? "actionType")
            Prelude.<*> (x Data..:? "resources" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AuthInfo where
  hashWithSalt _salt AuthInfo' {..} =
    _salt `Prelude.hashWithSalt` actionType
      `Prelude.hashWithSalt` resources

instance Prelude.NFData AuthInfo where
  rnf AuthInfo' {..} =
    Prelude.rnf actionType
      `Prelude.seq` Prelude.rnf resources

instance Data.ToJSON AuthInfo where
  toJSON AuthInfo' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("actionType" Data..=) Prelude.<$> actionType,
            Prelude.Just ("resources" Data..= resources)
          ]
      )
