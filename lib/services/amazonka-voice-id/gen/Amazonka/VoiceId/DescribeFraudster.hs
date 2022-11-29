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
-- Module      : Amazonka.VoiceId.DescribeFraudster
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified fraudster.
module Amazonka.VoiceId.DescribeFraudster
  ( -- * Creating a Request
    DescribeFraudster (..),
    newDescribeFraudster,

    -- * Request Lenses
    describeFraudster_domainId,
    describeFraudster_fraudsterId,

    -- * Destructuring the Response
    DescribeFraudsterResponse (..),
    newDescribeFraudsterResponse,

    -- * Response Lenses
    describeFraudsterResponse_fraudster,
    describeFraudsterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VoiceId.Types

-- | /See:/ 'newDescribeFraudster' smart constructor.
data DescribeFraudster = DescribeFraudster'
  { -- | The identifier of the domain containing the fraudster.
    domainId :: Prelude.Text,
    -- | The identifier of the fraudster you are describing.
    fraudsterId :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFraudster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainId', 'describeFraudster_domainId' - The identifier of the domain containing the fraudster.
--
-- 'fraudsterId', 'describeFraudster_fraudsterId' - The identifier of the fraudster you are describing.
newDescribeFraudster ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'fraudsterId'
  Prelude.Text ->
  DescribeFraudster
newDescribeFraudster pDomainId_ pFraudsterId_ =
  DescribeFraudster'
    { domainId = pDomainId_,
      fraudsterId = Core._Sensitive Lens.# pFraudsterId_
    }

-- | The identifier of the domain containing the fraudster.
describeFraudster_domainId :: Lens.Lens' DescribeFraudster Prelude.Text
describeFraudster_domainId = Lens.lens (\DescribeFraudster' {domainId} -> domainId) (\s@DescribeFraudster' {} a -> s {domainId = a} :: DescribeFraudster)

-- | The identifier of the fraudster you are describing.
describeFraudster_fraudsterId :: Lens.Lens' DescribeFraudster Prelude.Text
describeFraudster_fraudsterId = Lens.lens (\DescribeFraudster' {fraudsterId} -> fraudsterId) (\s@DescribeFraudster' {} a -> s {fraudsterId = a} :: DescribeFraudster) Prelude.. Core._Sensitive

instance Core.AWSRequest DescribeFraudster where
  type
    AWSResponse DescribeFraudster =
      DescribeFraudsterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFraudsterResponse'
            Prelude.<$> (x Core..?> "Fraudster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFraudster where
  hashWithSalt _salt DescribeFraudster' {..} =
    _salt `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` fraudsterId

instance Prelude.NFData DescribeFraudster where
  rnf DescribeFraudster' {..} =
    Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf fraudsterId

instance Core.ToHeaders DescribeFraudster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("VoiceID.DescribeFraudster" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeFraudster where
  toJSON DescribeFraudster' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DomainId" Core..= domainId),
            Prelude.Just ("FraudsterId" Core..= fraudsterId)
          ]
      )

instance Core.ToPath DescribeFraudster where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeFraudster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFraudsterResponse' smart constructor.
data DescribeFraudsterResponse = DescribeFraudsterResponse'
  { -- | Information about the specified fraudster.
    fraudster :: Prelude.Maybe Fraudster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFraudsterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fraudster', 'describeFraudsterResponse_fraudster' - Information about the specified fraudster.
--
-- 'httpStatus', 'describeFraudsterResponse_httpStatus' - The response's http status code.
newDescribeFraudsterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFraudsterResponse
newDescribeFraudsterResponse pHttpStatus_ =
  DescribeFraudsterResponse'
    { fraudster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the specified fraudster.
describeFraudsterResponse_fraudster :: Lens.Lens' DescribeFraudsterResponse (Prelude.Maybe Fraudster)
describeFraudsterResponse_fraudster = Lens.lens (\DescribeFraudsterResponse' {fraudster} -> fraudster) (\s@DescribeFraudsterResponse' {} a -> s {fraudster = a} :: DescribeFraudsterResponse)

-- | The response's http status code.
describeFraudsterResponse_httpStatus :: Lens.Lens' DescribeFraudsterResponse Prelude.Int
describeFraudsterResponse_httpStatus = Lens.lens (\DescribeFraudsterResponse' {httpStatus} -> httpStatus) (\s@DescribeFraudsterResponse' {} a -> s {httpStatus = a} :: DescribeFraudsterResponse)

instance Prelude.NFData DescribeFraudsterResponse where
  rnf DescribeFraudsterResponse' {..} =
    Prelude.rnf fraudster
      `Prelude.seq` Prelude.rnf httpStatus
