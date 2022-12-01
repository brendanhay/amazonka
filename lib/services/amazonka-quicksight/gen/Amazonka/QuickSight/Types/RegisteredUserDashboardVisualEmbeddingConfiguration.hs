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
-- Module      : Amazonka.QuickSight.Types.RegisteredUserDashboardVisualEmbeddingConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RegisteredUserDashboardVisualEmbeddingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DashboardVisualId

-- | The experience that you are embedding. You can use this object to
-- generate a url that embeds a visual into your application.
--
-- /See:/ 'newRegisteredUserDashboardVisualEmbeddingConfiguration' smart constructor.
data RegisteredUserDashboardVisualEmbeddingConfiguration = RegisteredUserDashboardVisualEmbeddingConfiguration'
  { -- | The visual ID for the visual that you want the user to embed. This ID is
    -- included in the output URL. When the URL in response is accessed, Amazon
    -- QuickSight renders this visual.
    --
    -- The Amazon Resource Name (ARN) of the dashboard that the visual belongs
    -- to must be included in the @AuthorizedResourceArns@ parameter.
    -- Otherwise, the request will fail with @InvalidParameterValueException@.
    initialDashboardVisualId :: DashboardVisualId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisteredUserDashboardVisualEmbeddingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'initialDashboardVisualId', 'registeredUserDashboardVisualEmbeddingConfiguration_initialDashboardVisualId' - The visual ID for the visual that you want the user to embed. This ID is
-- included in the output URL. When the URL in response is accessed, Amazon
-- QuickSight renders this visual.
--
-- The Amazon Resource Name (ARN) of the dashboard that the visual belongs
-- to must be included in the @AuthorizedResourceArns@ parameter.
-- Otherwise, the request will fail with @InvalidParameterValueException@.
newRegisteredUserDashboardVisualEmbeddingConfiguration ::
  -- | 'initialDashboardVisualId'
  DashboardVisualId ->
  RegisteredUserDashboardVisualEmbeddingConfiguration
newRegisteredUserDashboardVisualEmbeddingConfiguration
  pInitialDashboardVisualId_ =
    RegisteredUserDashboardVisualEmbeddingConfiguration'
      { initialDashboardVisualId =
          pInitialDashboardVisualId_
      }

-- | The visual ID for the visual that you want the user to embed. This ID is
-- included in the output URL. When the URL in response is accessed, Amazon
-- QuickSight renders this visual.
--
-- The Amazon Resource Name (ARN) of the dashboard that the visual belongs
-- to must be included in the @AuthorizedResourceArns@ parameter.
-- Otherwise, the request will fail with @InvalidParameterValueException@.
registeredUserDashboardVisualEmbeddingConfiguration_initialDashboardVisualId :: Lens.Lens' RegisteredUserDashboardVisualEmbeddingConfiguration DashboardVisualId
registeredUserDashboardVisualEmbeddingConfiguration_initialDashboardVisualId = Lens.lens (\RegisteredUserDashboardVisualEmbeddingConfiguration' {initialDashboardVisualId} -> initialDashboardVisualId) (\s@RegisteredUserDashboardVisualEmbeddingConfiguration' {} a -> s {initialDashboardVisualId = a} :: RegisteredUserDashboardVisualEmbeddingConfiguration)

instance
  Prelude.Hashable
    RegisteredUserDashboardVisualEmbeddingConfiguration
  where
  hashWithSalt
    _salt
    RegisteredUserDashboardVisualEmbeddingConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` initialDashboardVisualId

instance
  Prelude.NFData
    RegisteredUserDashboardVisualEmbeddingConfiguration
  where
  rnf
    RegisteredUserDashboardVisualEmbeddingConfiguration' {..} =
      Prelude.rnf initialDashboardVisualId

instance
  Core.ToJSON
    RegisteredUserDashboardVisualEmbeddingConfiguration
  where
  toJSON
    RegisteredUserDashboardVisualEmbeddingConfiguration' {..} =
      Core.object
        ( Prelude.catMaybes
            [ Prelude.Just
                ( "InitialDashboardVisualId"
                    Core..= initialDashboardVisualId
                )
            ]
        )
