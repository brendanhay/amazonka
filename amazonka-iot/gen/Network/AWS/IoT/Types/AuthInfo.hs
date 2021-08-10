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
-- Module      : Network.AWS.IoT.Types.AuthInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuthInfo where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.ActionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
authInfo_resources = Lens.lens (\AuthInfo' {resources} -> resources) (\s@AuthInfo' {} a -> s {resources = a} :: AuthInfo) Prelude.. Lens._Coerce

instance Core.FromJSON AuthInfo where
  parseJSON =
    Core.withObject
      "AuthInfo"
      ( \x ->
          AuthInfo'
            Prelude.<$> (x Core..:? "actionType")
            Prelude.<*> (x Core..:? "resources" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable AuthInfo

instance Prelude.NFData AuthInfo

instance Core.ToJSON AuthInfo where
  toJSON AuthInfo' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("actionType" Core..=) Prelude.<$> actionType,
            Prelude.Just ("resources" Core..= resources)
          ]
      )
