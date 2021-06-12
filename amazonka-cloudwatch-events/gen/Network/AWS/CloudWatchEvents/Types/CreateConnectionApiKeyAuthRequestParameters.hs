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
-- Module      : Network.AWS.CloudWatchEvents.Types.CreateConnectionApiKeyAuthRequestParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.CreateConnectionApiKeyAuthRequestParameters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains the API key authorization parameters for the connection.
--
-- /See:/ 'newCreateConnectionApiKeyAuthRequestParameters' smart constructor.
data CreateConnectionApiKeyAuthRequestParameters = CreateConnectionApiKeyAuthRequestParameters'
  { -- | The name of the API key to use for authorization.
    apiKeyName :: Core.Text,
    -- | The value for the API key to use for authorization.
    apiKeyValue :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateConnectionApiKeyAuthRequestParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiKeyName', 'createConnectionApiKeyAuthRequestParameters_apiKeyName' - The name of the API key to use for authorization.
--
-- 'apiKeyValue', 'createConnectionApiKeyAuthRequestParameters_apiKeyValue' - The value for the API key to use for authorization.
newCreateConnectionApiKeyAuthRequestParameters ::
  -- | 'apiKeyName'
  Core.Text ->
  -- | 'apiKeyValue'
  Core.Text ->
  CreateConnectionApiKeyAuthRequestParameters
newCreateConnectionApiKeyAuthRequestParameters
  pApiKeyName_
  pApiKeyValue_ =
    CreateConnectionApiKeyAuthRequestParameters'
      { apiKeyName =
          pApiKeyName_,
        apiKeyValue = pApiKeyValue_
      }

-- | The name of the API key to use for authorization.
createConnectionApiKeyAuthRequestParameters_apiKeyName :: Lens.Lens' CreateConnectionApiKeyAuthRequestParameters Core.Text
createConnectionApiKeyAuthRequestParameters_apiKeyName = Lens.lens (\CreateConnectionApiKeyAuthRequestParameters' {apiKeyName} -> apiKeyName) (\s@CreateConnectionApiKeyAuthRequestParameters' {} a -> s {apiKeyName = a} :: CreateConnectionApiKeyAuthRequestParameters)

-- | The value for the API key to use for authorization.
createConnectionApiKeyAuthRequestParameters_apiKeyValue :: Lens.Lens' CreateConnectionApiKeyAuthRequestParameters Core.Text
createConnectionApiKeyAuthRequestParameters_apiKeyValue = Lens.lens (\CreateConnectionApiKeyAuthRequestParameters' {apiKeyValue} -> apiKeyValue) (\s@CreateConnectionApiKeyAuthRequestParameters' {} a -> s {apiKeyValue = a} :: CreateConnectionApiKeyAuthRequestParameters)

instance
  Core.Hashable
    CreateConnectionApiKeyAuthRequestParameters

instance
  Core.NFData
    CreateConnectionApiKeyAuthRequestParameters

instance
  Core.ToJSON
    CreateConnectionApiKeyAuthRequestParameters
  where
  toJSON
    CreateConnectionApiKeyAuthRequestParameters' {..} =
      Core.object
        ( Core.catMaybes
            [ Core.Just ("ApiKeyName" Core..= apiKeyName),
              Core.Just ("ApiKeyValue" Core..= apiKeyValue)
            ]
        )
