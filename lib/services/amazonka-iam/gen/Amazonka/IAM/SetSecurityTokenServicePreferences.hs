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
-- Module      : Amazonka.IAM.SetSecurityTokenServicePreferences
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the specified version of the global endpoint token as the token
-- version used for the Amazon Web Services account.
--
-- By default, Security Token Service (STS) is available as a global
-- service, and all STS requests go to a single endpoint at
-- @https:\/\/sts.amazonaws.com@. Amazon Web Services recommends using
-- Regional STS endpoints to reduce latency, build in redundancy, and
-- increase session token availability. For information about Regional
-- endpoints for STS, see
-- <https://docs.aws.amazon.com/general/latest/gr/sts.html Security Token Service endpoints and quotas>
-- in the /Amazon Web Services General Reference/.
--
-- If you make an STS call to the global endpoint, the resulting session
-- tokens might be valid in some Regions but not others. It depends on the
-- version that is set in this operation. Version 1 tokens are valid only
-- in Amazon Web Services Regions that are available by default. These
-- tokens do not work in manually enabled Regions, such as Asia Pacific
-- (Hong Kong). Version 2 tokens are valid in all Regions. However, version
-- 2 tokens are longer and might affect systems where you temporarily store
-- tokens. For information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and deactivating STS in an Amazon Web Services Region>
-- in the /IAM User Guide/.
--
-- To view the current session token version, see the
-- @GlobalEndpointTokenVersion@ entry in the response of the
-- GetAccountSummary operation.
module Amazonka.IAM.SetSecurityTokenServicePreferences
  ( -- * Creating a Request
    SetSecurityTokenServicePreferences (..),
    newSetSecurityTokenServicePreferences,

    -- * Request Lenses
    setSecurityTokenServicePreferences_globalEndpointTokenVersion,

    -- * Destructuring the Response
    SetSecurityTokenServicePreferencesResponse (..),
    newSetSecurityTokenServicePreferencesResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSetSecurityTokenServicePreferences' smart constructor.
data SetSecurityTokenServicePreferences = SetSecurityTokenServicePreferences'
  { -- | The version of the global endpoint token. Version 1 tokens are valid
    -- only in Amazon Web Services Regions that are available by default. These
    -- tokens do not work in manually enabled Regions, such as Asia Pacific
    -- (Hong Kong). Version 2 tokens are valid in all Regions. However, version
    -- 2 tokens are longer and might affect systems where you temporarily store
    -- tokens.
    --
    -- For information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and deactivating STS in an Amazon Web Services Region>
    -- in the /IAM User Guide/.
    globalEndpointTokenVersion :: GlobalEndpointTokenVersion
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetSecurityTokenServicePreferences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalEndpointTokenVersion', 'setSecurityTokenServicePreferences_globalEndpointTokenVersion' - The version of the global endpoint token. Version 1 tokens are valid
-- only in Amazon Web Services Regions that are available by default. These
-- tokens do not work in manually enabled Regions, such as Asia Pacific
-- (Hong Kong). Version 2 tokens are valid in all Regions. However, version
-- 2 tokens are longer and might affect systems where you temporarily store
-- tokens.
--
-- For information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and deactivating STS in an Amazon Web Services Region>
-- in the /IAM User Guide/.
newSetSecurityTokenServicePreferences ::
  -- | 'globalEndpointTokenVersion'
  GlobalEndpointTokenVersion ->
  SetSecurityTokenServicePreferences
newSetSecurityTokenServicePreferences
  pGlobalEndpointTokenVersion_ =
    SetSecurityTokenServicePreferences'
      { globalEndpointTokenVersion =
          pGlobalEndpointTokenVersion_
      }

-- | The version of the global endpoint token. Version 1 tokens are valid
-- only in Amazon Web Services Regions that are available by default. These
-- tokens do not work in manually enabled Regions, such as Asia Pacific
-- (Hong Kong). Version 2 tokens are valid in all Regions. However, version
-- 2 tokens are longer and might affect systems where you temporarily store
-- tokens.
--
-- For information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and deactivating STS in an Amazon Web Services Region>
-- in the /IAM User Guide/.
setSecurityTokenServicePreferences_globalEndpointTokenVersion :: Lens.Lens' SetSecurityTokenServicePreferences GlobalEndpointTokenVersion
setSecurityTokenServicePreferences_globalEndpointTokenVersion = Lens.lens (\SetSecurityTokenServicePreferences' {globalEndpointTokenVersion} -> globalEndpointTokenVersion) (\s@SetSecurityTokenServicePreferences' {} a -> s {globalEndpointTokenVersion = a} :: SetSecurityTokenServicePreferences)

instance
  Core.AWSRequest
    SetSecurityTokenServicePreferences
  where
  type
    AWSResponse SetSecurityTokenServicePreferences =
      SetSecurityTokenServicePreferencesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      SetSecurityTokenServicePreferencesResponse'

instance
  Prelude.Hashable
    SetSecurityTokenServicePreferences
  where
  hashWithSalt
    _salt
    SetSecurityTokenServicePreferences' {..} =
      _salt
        `Prelude.hashWithSalt` globalEndpointTokenVersion

instance
  Prelude.NFData
    SetSecurityTokenServicePreferences
  where
  rnf SetSecurityTokenServicePreferences' {..} =
    Prelude.rnf globalEndpointTokenVersion

instance
  Data.ToHeaders
    SetSecurityTokenServicePreferences
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    SetSecurityTokenServicePreferences
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    SetSecurityTokenServicePreferences
  where
  toQuery SetSecurityTokenServicePreferences' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "SetSecurityTokenServicePreferences" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "GlobalEndpointTokenVersion"
          Data.=: globalEndpointTokenVersion
      ]

-- | /See:/ 'newSetSecurityTokenServicePreferencesResponse' smart constructor.
data SetSecurityTokenServicePreferencesResponse = SetSecurityTokenServicePreferencesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetSecurityTokenServicePreferencesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetSecurityTokenServicePreferencesResponse ::
  SetSecurityTokenServicePreferencesResponse
newSetSecurityTokenServicePreferencesResponse =
  SetSecurityTokenServicePreferencesResponse'

instance
  Prelude.NFData
    SetSecurityTokenServicePreferencesResponse
  where
  rnf _ = ()
