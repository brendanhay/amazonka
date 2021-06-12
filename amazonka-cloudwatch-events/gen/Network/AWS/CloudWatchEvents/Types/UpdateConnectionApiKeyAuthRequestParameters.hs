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
-- Module      : Network.AWS.CloudWatchEvents.Types.UpdateConnectionApiKeyAuthRequestParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.UpdateConnectionApiKeyAuthRequestParameters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains the API key authorization parameters to use to update the
-- connection.
--
-- /See:/ 'newUpdateConnectionApiKeyAuthRequestParameters' smart constructor.
data UpdateConnectionApiKeyAuthRequestParameters = UpdateConnectionApiKeyAuthRequestParameters'
  { -- | The value associated with teh API key to use for authorization.
    apiKeyValue :: Core.Maybe Core.Text,
    -- | The name of the API key to use for authorization.
    apiKeyName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateConnectionApiKeyAuthRequestParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiKeyValue', 'updateConnectionApiKeyAuthRequestParameters_apiKeyValue' - The value associated with teh API key to use for authorization.
--
-- 'apiKeyName', 'updateConnectionApiKeyAuthRequestParameters_apiKeyName' - The name of the API key to use for authorization.
newUpdateConnectionApiKeyAuthRequestParameters ::
  UpdateConnectionApiKeyAuthRequestParameters
newUpdateConnectionApiKeyAuthRequestParameters =
  UpdateConnectionApiKeyAuthRequestParameters'
    { apiKeyValue =
        Core.Nothing,
      apiKeyName = Core.Nothing
    }

-- | The value associated with teh API key to use for authorization.
updateConnectionApiKeyAuthRequestParameters_apiKeyValue :: Lens.Lens' UpdateConnectionApiKeyAuthRequestParameters (Core.Maybe Core.Text)
updateConnectionApiKeyAuthRequestParameters_apiKeyValue = Lens.lens (\UpdateConnectionApiKeyAuthRequestParameters' {apiKeyValue} -> apiKeyValue) (\s@UpdateConnectionApiKeyAuthRequestParameters' {} a -> s {apiKeyValue = a} :: UpdateConnectionApiKeyAuthRequestParameters)

-- | The name of the API key to use for authorization.
updateConnectionApiKeyAuthRequestParameters_apiKeyName :: Lens.Lens' UpdateConnectionApiKeyAuthRequestParameters (Core.Maybe Core.Text)
updateConnectionApiKeyAuthRequestParameters_apiKeyName = Lens.lens (\UpdateConnectionApiKeyAuthRequestParameters' {apiKeyName} -> apiKeyName) (\s@UpdateConnectionApiKeyAuthRequestParameters' {} a -> s {apiKeyName = a} :: UpdateConnectionApiKeyAuthRequestParameters)

instance
  Core.Hashable
    UpdateConnectionApiKeyAuthRequestParameters

instance
  Core.NFData
    UpdateConnectionApiKeyAuthRequestParameters

instance
  Core.ToJSON
    UpdateConnectionApiKeyAuthRequestParameters
  where
  toJSON
    UpdateConnectionApiKeyAuthRequestParameters' {..} =
      Core.object
        ( Core.catMaybes
            [ ("ApiKeyValue" Core..=) Core.<$> apiKeyValue,
              ("ApiKeyName" Core..=) Core.<$> apiKeyName
            ]
        )
