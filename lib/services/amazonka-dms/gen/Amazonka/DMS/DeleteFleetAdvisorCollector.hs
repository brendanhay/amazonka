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
-- Module      : Amazonka.DMS.DeleteFleetAdvisorCollector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Fleet Advisor collector.
module Amazonka.DMS.DeleteFleetAdvisorCollector
  ( -- * Creating a Request
    DeleteFleetAdvisorCollector (..),
    newDeleteFleetAdvisorCollector,

    -- * Request Lenses
    deleteFleetAdvisorCollector_collectorReferencedId,

    -- * Destructuring the Response
    DeleteFleetAdvisorCollectorResponse (..),
    newDeleteFleetAdvisorCollectorResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteFleetAdvisorCollector' smart constructor.
data DeleteFleetAdvisorCollector = DeleteFleetAdvisorCollector'
  { -- | The reference ID of the Fleet Advisor collector to delete.
    collectorReferencedId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFleetAdvisorCollector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectorReferencedId', 'deleteFleetAdvisorCollector_collectorReferencedId' - The reference ID of the Fleet Advisor collector to delete.
newDeleteFleetAdvisorCollector ::
  -- | 'collectorReferencedId'
  Prelude.Text ->
  DeleteFleetAdvisorCollector
newDeleteFleetAdvisorCollector
  pCollectorReferencedId_ =
    DeleteFleetAdvisorCollector'
      { collectorReferencedId =
          pCollectorReferencedId_
      }

-- | The reference ID of the Fleet Advisor collector to delete.
deleteFleetAdvisorCollector_collectorReferencedId :: Lens.Lens' DeleteFleetAdvisorCollector Prelude.Text
deleteFleetAdvisorCollector_collectorReferencedId = Lens.lens (\DeleteFleetAdvisorCollector' {collectorReferencedId} -> collectorReferencedId) (\s@DeleteFleetAdvisorCollector' {} a -> s {collectorReferencedId = a} :: DeleteFleetAdvisorCollector)

instance Core.AWSRequest DeleteFleetAdvisorCollector where
  type
    AWSResponse DeleteFleetAdvisorCollector =
      DeleteFleetAdvisorCollectorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteFleetAdvisorCollectorResponse'

instance Prelude.Hashable DeleteFleetAdvisorCollector where
  hashWithSalt _salt DeleteFleetAdvisorCollector' {..} =
    _salt `Prelude.hashWithSalt` collectorReferencedId

instance Prelude.NFData DeleteFleetAdvisorCollector where
  rnf DeleteFleetAdvisorCollector' {..} =
    Prelude.rnf collectorReferencedId

instance Data.ToHeaders DeleteFleetAdvisorCollector where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.DeleteFleetAdvisorCollector" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteFleetAdvisorCollector where
  toJSON DeleteFleetAdvisorCollector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "CollectorReferencedId"
                  Data..= collectorReferencedId
              )
          ]
      )

instance Data.ToPath DeleteFleetAdvisorCollector where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteFleetAdvisorCollector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFleetAdvisorCollectorResponse' smart constructor.
data DeleteFleetAdvisorCollectorResponse = DeleteFleetAdvisorCollectorResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFleetAdvisorCollectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteFleetAdvisorCollectorResponse ::
  DeleteFleetAdvisorCollectorResponse
newDeleteFleetAdvisorCollectorResponse =
  DeleteFleetAdvisorCollectorResponse'

instance
  Prelude.NFData
    DeleteFleetAdvisorCollectorResponse
  where
  rnf _ = ()
