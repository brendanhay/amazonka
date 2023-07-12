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
-- Module      : Amazonka.EC2.ReleaseHosts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- When you no longer want to use an On-Demand Dedicated Host it can be
-- released. On-Demand billing is stopped and the host goes into @released@
-- state. The host ID of Dedicated Hosts that have been released can no
-- longer be specified in another request, for example, to modify the host.
-- You must stop or terminate all instances on a host before it can be
-- released.
--
-- When Dedicated Hosts are released, it may take some time for them to
-- stop counting toward your limit and you may receive capacity errors when
-- trying to allocate new Dedicated Hosts. Wait a few minutes and then try
-- again.
--
-- Released hosts still appear in a DescribeHosts response.
module Amazonka.EC2.ReleaseHosts
  ( -- * Creating a Request
    ReleaseHosts (..),
    newReleaseHosts,

    -- * Request Lenses
    releaseHosts_hostIds,

    -- * Destructuring the Response
    ReleaseHostsResponse (..),
    newReleaseHostsResponse,

    -- * Response Lenses
    releaseHostsResponse_successful,
    releaseHostsResponse_unsuccessful,
    releaseHostsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newReleaseHosts' smart constructor.
data ReleaseHosts = ReleaseHosts'
  { -- | The IDs of the Dedicated Hosts to release.
    hostIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReleaseHosts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostIds', 'releaseHosts_hostIds' - The IDs of the Dedicated Hosts to release.
newReleaseHosts ::
  ReleaseHosts
newReleaseHosts =
  ReleaseHosts' {hostIds = Prelude.mempty}

-- | The IDs of the Dedicated Hosts to release.
releaseHosts_hostIds :: Lens.Lens' ReleaseHosts [Prelude.Text]
releaseHosts_hostIds = Lens.lens (\ReleaseHosts' {hostIds} -> hostIds) (\s@ReleaseHosts' {} a -> s {hostIds = a} :: ReleaseHosts) Prelude.. Lens.coerced

instance Core.AWSRequest ReleaseHosts where
  type AWSResponse ReleaseHosts = ReleaseHostsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ReleaseHostsResponse'
            Prelude.<$> ( x
                            Data..@? "successful"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> ( x
                            Data..@? "unsuccessful"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ReleaseHosts where
  hashWithSalt _salt ReleaseHosts' {..} =
    _salt `Prelude.hashWithSalt` hostIds

instance Prelude.NFData ReleaseHosts where
  rnf ReleaseHosts' {..} = Prelude.rnf hostIds

instance Data.ToHeaders ReleaseHosts where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ReleaseHosts where
  toPath = Prelude.const "/"

instance Data.ToQuery ReleaseHosts where
  toQuery ReleaseHosts' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ReleaseHosts" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        Data.toQueryList "HostId" hostIds
      ]

-- | /See:/ 'newReleaseHostsResponse' smart constructor.
data ReleaseHostsResponse = ReleaseHostsResponse'
  { -- | The IDs of the Dedicated Hosts that were successfully released.
    successful :: Prelude.Maybe [Prelude.Text],
    -- | The IDs of the Dedicated Hosts that could not be released, including an
    -- error message.
    unsuccessful :: Prelude.Maybe [UnsuccessfulItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReleaseHostsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'successful', 'releaseHostsResponse_successful' - The IDs of the Dedicated Hosts that were successfully released.
--
-- 'unsuccessful', 'releaseHostsResponse_unsuccessful' - The IDs of the Dedicated Hosts that could not be released, including an
-- error message.
--
-- 'httpStatus', 'releaseHostsResponse_httpStatus' - The response's http status code.
newReleaseHostsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ReleaseHostsResponse
newReleaseHostsResponse pHttpStatus_ =
  ReleaseHostsResponse'
    { successful = Prelude.Nothing,
      unsuccessful = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IDs of the Dedicated Hosts that were successfully released.
releaseHostsResponse_successful :: Lens.Lens' ReleaseHostsResponse (Prelude.Maybe [Prelude.Text])
releaseHostsResponse_successful = Lens.lens (\ReleaseHostsResponse' {successful} -> successful) (\s@ReleaseHostsResponse' {} a -> s {successful = a} :: ReleaseHostsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the Dedicated Hosts that could not be released, including an
-- error message.
releaseHostsResponse_unsuccessful :: Lens.Lens' ReleaseHostsResponse (Prelude.Maybe [UnsuccessfulItem])
releaseHostsResponse_unsuccessful = Lens.lens (\ReleaseHostsResponse' {unsuccessful} -> unsuccessful) (\s@ReleaseHostsResponse' {} a -> s {unsuccessful = a} :: ReleaseHostsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
releaseHostsResponse_httpStatus :: Lens.Lens' ReleaseHostsResponse Prelude.Int
releaseHostsResponse_httpStatus = Lens.lens (\ReleaseHostsResponse' {httpStatus} -> httpStatus) (\s@ReleaseHostsResponse' {} a -> s {httpStatus = a} :: ReleaseHostsResponse)

instance Prelude.NFData ReleaseHostsResponse where
  rnf ReleaseHostsResponse' {..} =
    Prelude.rnf successful
      `Prelude.seq` Prelude.rnf unsuccessful
      `Prelude.seq` Prelude.rnf httpStatus
