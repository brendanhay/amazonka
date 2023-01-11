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
-- Module      : Amazonka.SESV2.PutConfigurationSetVdmOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specify VDM preferences for email that you send using the configuration
-- set.
--
-- You can execute this operation no more than once per second.
module Amazonka.SESV2.PutConfigurationSetVdmOptions
  ( -- * Creating a Request
    PutConfigurationSetVdmOptions (..),
    newPutConfigurationSetVdmOptions,

    -- * Request Lenses
    putConfigurationSetVdmOptions_vdmOptions,
    putConfigurationSetVdmOptions_configurationSetName,

    -- * Destructuring the Response
    PutConfigurationSetVdmOptionsResponse (..),
    newPutConfigurationSetVdmOptionsResponse,

    -- * Response Lenses
    putConfigurationSetVdmOptionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | A request to add specific VDM settings to a configuration set.
--
-- /See:/ 'newPutConfigurationSetVdmOptions' smart constructor.
data PutConfigurationSetVdmOptions = PutConfigurationSetVdmOptions'
  { -- | The VDM options to apply to the configuration set.
    vdmOptions :: Prelude.Maybe VdmOptions,
    -- | The name of the configuration set.
    configurationSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutConfigurationSetVdmOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vdmOptions', 'putConfigurationSetVdmOptions_vdmOptions' - The VDM options to apply to the configuration set.
--
-- 'configurationSetName', 'putConfigurationSetVdmOptions_configurationSetName' - The name of the configuration set.
newPutConfigurationSetVdmOptions ::
  -- | 'configurationSetName'
  Prelude.Text ->
  PutConfigurationSetVdmOptions
newPutConfigurationSetVdmOptions
  pConfigurationSetName_ =
    PutConfigurationSetVdmOptions'
      { vdmOptions =
          Prelude.Nothing,
        configurationSetName =
          pConfigurationSetName_
      }

-- | The VDM options to apply to the configuration set.
putConfigurationSetVdmOptions_vdmOptions :: Lens.Lens' PutConfigurationSetVdmOptions (Prelude.Maybe VdmOptions)
putConfigurationSetVdmOptions_vdmOptions = Lens.lens (\PutConfigurationSetVdmOptions' {vdmOptions} -> vdmOptions) (\s@PutConfigurationSetVdmOptions' {} a -> s {vdmOptions = a} :: PutConfigurationSetVdmOptions)

-- | The name of the configuration set.
putConfigurationSetVdmOptions_configurationSetName :: Lens.Lens' PutConfigurationSetVdmOptions Prelude.Text
putConfigurationSetVdmOptions_configurationSetName = Lens.lens (\PutConfigurationSetVdmOptions' {configurationSetName} -> configurationSetName) (\s@PutConfigurationSetVdmOptions' {} a -> s {configurationSetName = a} :: PutConfigurationSetVdmOptions)

instance
  Core.AWSRequest
    PutConfigurationSetVdmOptions
  where
  type
    AWSResponse PutConfigurationSetVdmOptions =
      PutConfigurationSetVdmOptionsResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutConfigurationSetVdmOptionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutConfigurationSetVdmOptions
  where
  hashWithSalt _salt PutConfigurationSetVdmOptions' {..} =
    _salt `Prelude.hashWithSalt` vdmOptions
      `Prelude.hashWithSalt` configurationSetName

instance Prelude.NFData PutConfigurationSetVdmOptions where
  rnf PutConfigurationSetVdmOptions' {..} =
    Prelude.rnf vdmOptions
      `Prelude.seq` Prelude.rnf configurationSetName

instance Data.ToHeaders PutConfigurationSetVdmOptions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutConfigurationSetVdmOptions where
  toJSON PutConfigurationSetVdmOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [("VdmOptions" Data..=) Prelude.<$> vdmOptions]
      )

instance Data.ToPath PutConfigurationSetVdmOptions where
  toPath PutConfigurationSetVdmOptions' {..} =
    Prelude.mconcat
      [ "/v2/email/configuration-sets/",
        Data.toBS configurationSetName,
        "/vdm-options"
      ]

instance Data.ToQuery PutConfigurationSetVdmOptions where
  toQuery = Prelude.const Prelude.mempty

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
--
-- /See:/ 'newPutConfigurationSetVdmOptionsResponse' smart constructor.
data PutConfigurationSetVdmOptionsResponse = PutConfigurationSetVdmOptionsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutConfigurationSetVdmOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putConfigurationSetVdmOptionsResponse_httpStatus' - The response's http status code.
newPutConfigurationSetVdmOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutConfigurationSetVdmOptionsResponse
newPutConfigurationSetVdmOptionsResponse pHttpStatus_ =
  PutConfigurationSetVdmOptionsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putConfigurationSetVdmOptionsResponse_httpStatus :: Lens.Lens' PutConfigurationSetVdmOptionsResponse Prelude.Int
putConfigurationSetVdmOptionsResponse_httpStatus = Lens.lens (\PutConfigurationSetVdmOptionsResponse' {httpStatus} -> httpStatus) (\s@PutConfigurationSetVdmOptionsResponse' {} a -> s {httpStatus = a} :: PutConfigurationSetVdmOptionsResponse)

instance
  Prelude.NFData
    PutConfigurationSetVdmOptionsResponse
  where
  rnf PutConfigurationSetVdmOptionsResponse' {..} =
    Prelude.rnf httpStatus
