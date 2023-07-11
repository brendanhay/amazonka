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
-- Module      : Amazonka.QuickSight.Types.AnonymousUserDashboardVisualEmbeddingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AnonymousUserDashboardVisualEmbeddingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DashboardVisualId

-- | The experience that you are embedding. You can use this object to
-- generate a url that embeds a visual into your application.
--
-- /See:/ 'newAnonymousUserDashboardVisualEmbeddingConfiguration' smart constructor.
data AnonymousUserDashboardVisualEmbeddingConfiguration = AnonymousUserDashboardVisualEmbeddingConfiguration'
  { -- | The visual ID for the visual that you want the user to see. This ID is
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
-- Create a value of 'AnonymousUserDashboardVisualEmbeddingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'initialDashboardVisualId', 'anonymousUserDashboardVisualEmbeddingConfiguration_initialDashboardVisualId' - The visual ID for the visual that you want the user to see. This ID is
-- included in the output URL. When the URL in response is accessed, Amazon
-- QuickSight renders this visual.
--
-- The Amazon Resource Name (ARN) of the dashboard that the visual belongs
-- to must be included in the @AuthorizedResourceArns@ parameter.
-- Otherwise, the request will fail with @InvalidParameterValueException@.
newAnonymousUserDashboardVisualEmbeddingConfiguration ::
  -- | 'initialDashboardVisualId'
  DashboardVisualId ->
  AnonymousUserDashboardVisualEmbeddingConfiguration
newAnonymousUserDashboardVisualEmbeddingConfiguration
  pInitialDashboardVisualId_ =
    AnonymousUserDashboardVisualEmbeddingConfiguration'
      { initialDashboardVisualId =
          pInitialDashboardVisualId_
      }

-- | The visual ID for the visual that you want the user to see. This ID is
-- included in the output URL. When the URL in response is accessed, Amazon
-- QuickSight renders this visual.
--
-- The Amazon Resource Name (ARN) of the dashboard that the visual belongs
-- to must be included in the @AuthorizedResourceArns@ parameter.
-- Otherwise, the request will fail with @InvalidParameterValueException@.
anonymousUserDashboardVisualEmbeddingConfiguration_initialDashboardVisualId :: Lens.Lens' AnonymousUserDashboardVisualEmbeddingConfiguration DashboardVisualId
anonymousUserDashboardVisualEmbeddingConfiguration_initialDashboardVisualId = Lens.lens (\AnonymousUserDashboardVisualEmbeddingConfiguration' {initialDashboardVisualId} -> initialDashboardVisualId) (\s@AnonymousUserDashboardVisualEmbeddingConfiguration' {} a -> s {initialDashboardVisualId = a} :: AnonymousUserDashboardVisualEmbeddingConfiguration)

instance
  Prelude.Hashable
    AnonymousUserDashboardVisualEmbeddingConfiguration
  where
  hashWithSalt
    _salt
    AnonymousUserDashboardVisualEmbeddingConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` initialDashboardVisualId

instance
  Prelude.NFData
    AnonymousUserDashboardVisualEmbeddingConfiguration
  where
  rnf
    AnonymousUserDashboardVisualEmbeddingConfiguration' {..} =
      Prelude.rnf initialDashboardVisualId

instance
  Data.ToJSON
    AnonymousUserDashboardVisualEmbeddingConfiguration
  where
  toJSON
    AnonymousUserDashboardVisualEmbeddingConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [ Prelude.Just
                ( "InitialDashboardVisualId"
                    Data..= initialDashboardVisualId
                )
            ]
        )
