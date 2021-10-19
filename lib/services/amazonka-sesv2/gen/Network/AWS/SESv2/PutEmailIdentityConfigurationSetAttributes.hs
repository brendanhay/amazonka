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
-- Module      : Network.AWS.SESv2.PutEmailIdentityConfigurationSetAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to associate a configuration set with an email identity.
module Network.AWS.SESv2.PutEmailIdentityConfigurationSetAttributes
  ( -- * Creating a Request
    PutEmailIdentityConfigurationSetAttributes (..),
    newPutEmailIdentityConfigurationSetAttributes,

    -- * Request Lenses
    putEmailIdentityConfigurationSetAttributes_configurationSetName,
    putEmailIdentityConfigurationSetAttributes_emailIdentity,

    -- * Destructuring the Response
    PutEmailIdentityConfigurationSetAttributesResponse (..),
    newPutEmailIdentityConfigurationSetAttributesResponse,

    -- * Response Lenses
    putEmailIdentityConfigurationSetAttributesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

-- | A request to associate a configuration set with an email identity.
--
-- /See:/ 'newPutEmailIdentityConfigurationSetAttributes' smart constructor.
data PutEmailIdentityConfigurationSetAttributes = PutEmailIdentityConfigurationSetAttributes'
  { -- | The configuration set to associate with an email identity.
    configurationSetName :: Prelude.Maybe Prelude.Text,
    -- | The email address or domain to associate with a configuration set.
    emailIdentity :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutEmailIdentityConfigurationSetAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSetName', 'putEmailIdentityConfigurationSetAttributes_configurationSetName' - The configuration set to associate with an email identity.
--
-- 'emailIdentity', 'putEmailIdentityConfigurationSetAttributes_emailIdentity' - The email address or domain to associate with a configuration set.
newPutEmailIdentityConfigurationSetAttributes ::
  -- | 'emailIdentity'
  Prelude.Text ->
  PutEmailIdentityConfigurationSetAttributes
newPutEmailIdentityConfigurationSetAttributes
  pEmailIdentity_ =
    PutEmailIdentityConfigurationSetAttributes'
      { configurationSetName =
          Prelude.Nothing,
        emailIdentity = pEmailIdentity_
      }

-- | The configuration set to associate with an email identity.
putEmailIdentityConfigurationSetAttributes_configurationSetName :: Lens.Lens' PutEmailIdentityConfigurationSetAttributes (Prelude.Maybe Prelude.Text)
putEmailIdentityConfigurationSetAttributes_configurationSetName = Lens.lens (\PutEmailIdentityConfigurationSetAttributes' {configurationSetName} -> configurationSetName) (\s@PutEmailIdentityConfigurationSetAttributes' {} a -> s {configurationSetName = a} :: PutEmailIdentityConfigurationSetAttributes)

-- | The email address or domain to associate with a configuration set.
putEmailIdentityConfigurationSetAttributes_emailIdentity :: Lens.Lens' PutEmailIdentityConfigurationSetAttributes Prelude.Text
putEmailIdentityConfigurationSetAttributes_emailIdentity = Lens.lens (\PutEmailIdentityConfigurationSetAttributes' {emailIdentity} -> emailIdentity) (\s@PutEmailIdentityConfigurationSetAttributes' {} a -> s {emailIdentity = a} :: PutEmailIdentityConfigurationSetAttributes)

instance
  Core.AWSRequest
    PutEmailIdentityConfigurationSetAttributes
  where
  type
    AWSResponse
      PutEmailIdentityConfigurationSetAttributes =
      PutEmailIdentityConfigurationSetAttributesResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutEmailIdentityConfigurationSetAttributesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutEmailIdentityConfigurationSetAttributes

instance
  Prelude.NFData
    PutEmailIdentityConfigurationSetAttributes

instance
  Core.ToHeaders
    PutEmailIdentityConfigurationSetAttributes
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    PutEmailIdentityConfigurationSetAttributes
  where
  toJSON
    PutEmailIdentityConfigurationSetAttributes' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("ConfigurationSetName" Core..=)
                Prelude.<$> configurationSetName
            ]
        )

instance
  Core.ToPath
    PutEmailIdentityConfigurationSetAttributes
  where
  toPath
    PutEmailIdentityConfigurationSetAttributes' {..} =
      Prelude.mconcat
        [ "/v2/email/identities/",
          Core.toBS emailIdentity,
          "/configuration-set"
        ]

instance
  Core.ToQuery
    PutEmailIdentityConfigurationSetAttributes
  where
  toQuery = Prelude.const Prelude.mempty

-- | If the action is successful, the service sends back an HTTP 200 response
-- with an empty HTTP body.
--
-- /See:/ 'newPutEmailIdentityConfigurationSetAttributesResponse' smart constructor.
data PutEmailIdentityConfigurationSetAttributesResponse = PutEmailIdentityConfigurationSetAttributesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutEmailIdentityConfigurationSetAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putEmailIdentityConfigurationSetAttributesResponse_httpStatus' - The response's http status code.
newPutEmailIdentityConfigurationSetAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutEmailIdentityConfigurationSetAttributesResponse
newPutEmailIdentityConfigurationSetAttributesResponse
  pHttpStatus_ =
    PutEmailIdentityConfigurationSetAttributesResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
putEmailIdentityConfigurationSetAttributesResponse_httpStatus :: Lens.Lens' PutEmailIdentityConfigurationSetAttributesResponse Prelude.Int
putEmailIdentityConfigurationSetAttributesResponse_httpStatus = Lens.lens (\PutEmailIdentityConfigurationSetAttributesResponse' {httpStatus} -> httpStatus) (\s@PutEmailIdentityConfigurationSetAttributesResponse' {} a -> s {httpStatus = a} :: PutEmailIdentityConfigurationSetAttributesResponse)

instance
  Prelude.NFData
    PutEmailIdentityConfigurationSetAttributesResponse
