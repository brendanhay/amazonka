{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DMS.StartRecommendations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the analysis of your source database to provide recommendations
-- of target engines.
--
-- You can create recommendations for multiple source databases using
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_BatchStartRecommendations.html BatchStartRecommendations>.
module Amazonka.DMS.StartRecommendations
  ( -- * Creating a Request
    StartRecommendations (..),
    newStartRecommendations,

    -- * Request Lenses
    startRecommendations_databaseId,
    startRecommendations_settings,

    -- * Destructuring the Response
    StartRecommendationsResponse (..),
    newStartRecommendationsResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartRecommendations' smart constructor.
data StartRecommendations = StartRecommendations'
  { -- | The identifier of the source database to analyze and provide
    -- recommendations for.
    databaseId :: Prelude.Text,
    -- | The settings in JSON format that Fleet Advisor uses to determine target
    -- engine recommendations. These parameters include target instance sizing
    -- and availability and durability settings. For target instance sizing,
    -- Fleet Advisor supports the following two options: total capacity and
    -- resource utilization. For availability and durability, Fleet Advisor
    -- supports the following two options: production (Multi-AZ deployments)
    -- and Dev\/Test (Single-AZ deployments).
    settings :: RecommendationSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartRecommendations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databaseId', 'startRecommendations_databaseId' - The identifier of the source database to analyze and provide
-- recommendations for.
--
-- 'settings', 'startRecommendations_settings' - The settings in JSON format that Fleet Advisor uses to determine target
-- engine recommendations. These parameters include target instance sizing
-- and availability and durability settings. For target instance sizing,
-- Fleet Advisor supports the following two options: total capacity and
-- resource utilization. For availability and durability, Fleet Advisor
-- supports the following two options: production (Multi-AZ deployments)
-- and Dev\/Test (Single-AZ deployments).
newStartRecommendations ::
  -- | 'databaseId'
  Prelude.Text ->
  -- | 'settings'
  RecommendationSettings ->
  StartRecommendations
newStartRecommendations pDatabaseId_ pSettings_ =
  StartRecommendations'
    { databaseId = pDatabaseId_,
      settings = pSettings_
    }

-- | The identifier of the source database to analyze and provide
-- recommendations for.
startRecommendations_databaseId :: Lens.Lens' StartRecommendations Prelude.Text
startRecommendations_databaseId = Lens.lens (\StartRecommendations' {databaseId} -> databaseId) (\s@StartRecommendations' {} a -> s {databaseId = a} :: StartRecommendations)

-- | The settings in JSON format that Fleet Advisor uses to determine target
-- engine recommendations. These parameters include target instance sizing
-- and availability and durability settings. For target instance sizing,
-- Fleet Advisor supports the following two options: total capacity and
-- resource utilization. For availability and durability, Fleet Advisor
-- supports the following two options: production (Multi-AZ deployments)
-- and Dev\/Test (Single-AZ deployments).
startRecommendations_settings :: Lens.Lens' StartRecommendations RecommendationSettings
startRecommendations_settings = Lens.lens (\StartRecommendations' {settings} -> settings) (\s@StartRecommendations' {} a -> s {settings = a} :: StartRecommendations)

instance Core.AWSRequest StartRecommendations where
  type
    AWSResponse StartRecommendations =
      StartRecommendationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull StartRecommendationsResponse'

instance Prelude.Hashable StartRecommendations where
  hashWithSalt _salt StartRecommendations' {..} =
    _salt
      `Prelude.hashWithSalt` databaseId
      `Prelude.hashWithSalt` settings

instance Prelude.NFData StartRecommendations where
  rnf StartRecommendations' {..} =
    Prelude.rnf databaseId
      `Prelude.seq` Prelude.rnf settings

instance Data.ToHeaders StartRecommendations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.StartRecommendations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartRecommendations where
  toJSON StartRecommendations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DatabaseId" Data..= databaseId),
            Prelude.Just ("Settings" Data..= settings)
          ]
      )

instance Data.ToPath StartRecommendations where
  toPath = Prelude.const "/"

instance Data.ToQuery StartRecommendations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartRecommendationsResponse' smart constructor.
data StartRecommendationsResponse = StartRecommendationsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartRecommendationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStartRecommendationsResponse ::
  StartRecommendationsResponse
newStartRecommendationsResponse =
  StartRecommendationsResponse'

instance Prelude.NFData StartRecommendationsResponse where
  rnf _ = ()
