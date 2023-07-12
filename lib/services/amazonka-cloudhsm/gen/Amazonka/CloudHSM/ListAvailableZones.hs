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
-- Module      : Amazonka.CloudHSM.ListAvailableZones
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__. For more
-- information, see
-- <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs>,
-- the
-- <https://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide>,
-- and the
-- <https://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference>.
--
-- __For information about the current version of AWS CloudHSM__, see
-- <http://aws.amazon.com/cloudhsm/ AWS CloudHSM>, the
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide>,
-- and the
-- <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference>.
--
-- Lists the Availability Zones that have available AWS CloudHSM capacity.
module Amazonka.CloudHSM.ListAvailableZones
  ( -- * Creating a Request
    ListAvailableZones (..),
    newListAvailableZones,

    -- * Destructuring the Response
    ListAvailableZonesResponse (..),
    newListAvailableZonesResponse,

    -- * Response Lenses
    listAvailableZonesResponse_aZList,
    listAvailableZonesResponse_httpStatus,
  )
where

import Amazonka.CloudHSM.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the inputs for the ListAvailableZones action.
--
-- /See:/ 'newListAvailableZones' smart constructor.
data ListAvailableZones = ListAvailableZones'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAvailableZones' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newListAvailableZones ::
  ListAvailableZones
newListAvailableZones = ListAvailableZones'

instance Core.AWSRequest ListAvailableZones where
  type
    AWSResponse ListAvailableZones =
      ListAvailableZonesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAvailableZonesResponse'
            Prelude.<$> (x Data..?> "AZList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAvailableZones where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData ListAvailableZones where
  rnf _ = ()

instance Data.ToHeaders ListAvailableZones where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CloudHsmFrontendService.ListAvailableZones" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAvailableZones where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath ListAvailableZones where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAvailableZones where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAvailableZonesResponse' smart constructor.
data ListAvailableZonesResponse = ListAvailableZonesResponse'
  { -- | The list of Availability Zones that have available AWS CloudHSM
    -- capacity.
    aZList :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAvailableZonesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aZList', 'listAvailableZonesResponse_aZList' - The list of Availability Zones that have available AWS CloudHSM
-- capacity.
--
-- 'httpStatus', 'listAvailableZonesResponse_httpStatus' - The response's http status code.
newListAvailableZonesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAvailableZonesResponse
newListAvailableZonesResponse pHttpStatus_ =
  ListAvailableZonesResponse'
    { aZList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of Availability Zones that have available AWS CloudHSM
-- capacity.
listAvailableZonesResponse_aZList :: Lens.Lens' ListAvailableZonesResponse (Prelude.Maybe [Prelude.Text])
listAvailableZonesResponse_aZList = Lens.lens (\ListAvailableZonesResponse' {aZList} -> aZList) (\s@ListAvailableZonesResponse' {} a -> s {aZList = a} :: ListAvailableZonesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAvailableZonesResponse_httpStatus :: Lens.Lens' ListAvailableZonesResponse Prelude.Int
listAvailableZonesResponse_httpStatus = Lens.lens (\ListAvailableZonesResponse' {httpStatus} -> httpStatus) (\s@ListAvailableZonesResponse' {} a -> s {httpStatus = a} :: ListAvailableZonesResponse)

instance Prelude.NFData ListAvailableZonesResponse where
  rnf ListAvailableZonesResponse' {..} =
    Prelude.rnf aZList
      `Prelude.seq` Prelude.rnf httpStatus
