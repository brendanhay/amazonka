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
-- Module      : Network.AWS.CloudHSM.ListAvailableZones
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__. For more
-- information, see
-- <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs>,
-- the
-- <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide>,
-- and the
-- <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference>.
--
-- __For information about the current version of AWS CloudHSM__, see
-- <http://aws.amazon.com/cloudhsm/ AWS CloudHSM>, the
-- <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide>,
-- and the
-- <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference>.
--
-- Lists the Availability Zones that have available AWS CloudHSM capacity.
module Network.AWS.CloudHSM.ListAvailableZones
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

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAvailableZonesResponse'
            Prelude.<$> (x Core..?> "AZList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAvailableZones

instance Prelude.NFData ListAvailableZones

instance Core.ToHeaders ListAvailableZones where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CloudHsmFrontendService.ListAvailableZones" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListAvailableZones where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath ListAvailableZones where
  toPath = Prelude.const "/"

instance Core.ToQuery ListAvailableZones where
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
listAvailableZonesResponse_aZList = Lens.lens (\ListAvailableZonesResponse' {aZList} -> aZList) (\s@ListAvailableZonesResponse' {} a -> s {aZList = a} :: ListAvailableZonesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listAvailableZonesResponse_httpStatus :: Lens.Lens' ListAvailableZonesResponse Prelude.Int
listAvailableZonesResponse_httpStatus = Lens.lens (\ListAvailableZonesResponse' {httpStatus} -> httpStatus) (\s@ListAvailableZonesResponse' {} a -> s {httpStatus = a} :: ListAvailableZonesResponse)

instance Prelude.NFData ListAvailableZonesResponse
