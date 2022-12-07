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
-- Module      : Amazonka.Personalize.DeleteMetricAttribution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a metric attribution.
module Amazonka.Personalize.DeleteMetricAttribution
  ( -- * Creating a Request
    DeleteMetricAttribution (..),
    newDeleteMetricAttribution,

    -- * Request Lenses
    deleteMetricAttribution_metricAttributionArn,

    -- * Destructuring the Response
    DeleteMetricAttributionResponse (..),
    newDeleteMetricAttributionResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteMetricAttribution' smart constructor.
data DeleteMetricAttribution = DeleteMetricAttribution'
  { -- | The metric attribution\'s Amazon Resource Name (ARN).
    metricAttributionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMetricAttribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricAttributionArn', 'deleteMetricAttribution_metricAttributionArn' - The metric attribution\'s Amazon Resource Name (ARN).
newDeleteMetricAttribution ::
  -- | 'metricAttributionArn'
  Prelude.Text ->
  DeleteMetricAttribution
newDeleteMetricAttribution pMetricAttributionArn_ =
  DeleteMetricAttribution'
    { metricAttributionArn =
        pMetricAttributionArn_
    }

-- | The metric attribution\'s Amazon Resource Name (ARN).
deleteMetricAttribution_metricAttributionArn :: Lens.Lens' DeleteMetricAttribution Prelude.Text
deleteMetricAttribution_metricAttributionArn = Lens.lens (\DeleteMetricAttribution' {metricAttributionArn} -> metricAttributionArn) (\s@DeleteMetricAttribution' {} a -> s {metricAttributionArn = a} :: DeleteMetricAttribution)

instance Core.AWSRequest DeleteMetricAttribution where
  type
    AWSResponse DeleteMetricAttribution =
      DeleteMetricAttributionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteMetricAttributionResponse'

instance Prelude.Hashable DeleteMetricAttribution where
  hashWithSalt _salt DeleteMetricAttribution' {..} =
    _salt `Prelude.hashWithSalt` metricAttributionArn

instance Prelude.NFData DeleteMetricAttribution where
  rnf DeleteMetricAttribution' {..} =
    Prelude.rnf metricAttributionArn

instance Data.ToHeaders DeleteMetricAttribution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.DeleteMetricAttribution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteMetricAttribution where
  toJSON DeleteMetricAttribution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "metricAttributionArn"
                  Data..= metricAttributionArn
              )
          ]
      )

instance Data.ToPath DeleteMetricAttribution where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteMetricAttribution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMetricAttributionResponse' smart constructor.
data DeleteMetricAttributionResponse = DeleteMetricAttributionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMetricAttributionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteMetricAttributionResponse ::
  DeleteMetricAttributionResponse
newDeleteMetricAttributionResponse =
  DeleteMetricAttributionResponse'

instance
  Prelude.NFData
    DeleteMetricAttributionResponse
  where
  rnf _ = ()
