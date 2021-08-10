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
-- Module      : Network.AWS.SESv2.PutConfigurationSetSuppressionOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specify the account suppression list preferences for a configuration
-- set.
module Network.AWS.SESv2.PutConfigurationSetSuppressionOptions
  ( -- * Creating a Request
    PutConfigurationSetSuppressionOptions (..),
    newPutConfigurationSetSuppressionOptions,

    -- * Request Lenses
    putConfigurationSetSuppressionOptions_suppressedReasons,
    putConfigurationSetSuppressionOptions_configurationSetName,

    -- * Destructuring the Response
    PutConfigurationSetSuppressionOptionsResponse (..),
    newPutConfigurationSetSuppressionOptionsResponse,

    -- * Response Lenses
    putConfigurationSetSuppressionOptionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

-- | A request to change the account suppression list preferences for a
-- specific configuration set.
--
-- /See:/ 'newPutConfigurationSetSuppressionOptions' smart constructor.
data PutConfigurationSetSuppressionOptions = PutConfigurationSetSuppressionOptions'
  { -- | A list that contains the reasons that email addresses are automatically
    -- added to the suppression list for your account. This list can contain
    -- any or all of the following:
    --
    -- -   @COMPLAINT@ – Amazon SES adds an email address to the suppression
    --     list for your account when a message sent to that address results in
    --     a complaint.
    --
    -- -   @BOUNCE@ – Amazon SES adds an email address to the suppression list
    --     for your account when a message sent to that address results in a
    --     hard bounce.
    suppressedReasons :: Prelude.Maybe [SuppressionListReason],
    -- | The name of the configuration set that you want to change the
    -- suppression list preferences for.
    configurationSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutConfigurationSetSuppressionOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'suppressedReasons', 'putConfigurationSetSuppressionOptions_suppressedReasons' - A list that contains the reasons that email addresses are automatically
-- added to the suppression list for your account. This list can contain
-- any or all of the following:
--
-- -   @COMPLAINT@ – Amazon SES adds an email address to the suppression
--     list for your account when a message sent to that address results in
--     a complaint.
--
-- -   @BOUNCE@ – Amazon SES adds an email address to the suppression list
--     for your account when a message sent to that address results in a
--     hard bounce.
--
-- 'configurationSetName', 'putConfigurationSetSuppressionOptions_configurationSetName' - The name of the configuration set that you want to change the
-- suppression list preferences for.
newPutConfigurationSetSuppressionOptions ::
  -- | 'configurationSetName'
  Prelude.Text ->
  PutConfigurationSetSuppressionOptions
newPutConfigurationSetSuppressionOptions
  pConfigurationSetName_ =
    PutConfigurationSetSuppressionOptions'
      { suppressedReasons =
          Prelude.Nothing,
        configurationSetName =
          pConfigurationSetName_
      }

-- | A list that contains the reasons that email addresses are automatically
-- added to the suppression list for your account. This list can contain
-- any or all of the following:
--
-- -   @COMPLAINT@ – Amazon SES adds an email address to the suppression
--     list for your account when a message sent to that address results in
--     a complaint.
--
-- -   @BOUNCE@ – Amazon SES adds an email address to the suppression list
--     for your account when a message sent to that address results in a
--     hard bounce.
putConfigurationSetSuppressionOptions_suppressedReasons :: Lens.Lens' PutConfigurationSetSuppressionOptions (Prelude.Maybe [SuppressionListReason])
putConfigurationSetSuppressionOptions_suppressedReasons = Lens.lens (\PutConfigurationSetSuppressionOptions' {suppressedReasons} -> suppressedReasons) (\s@PutConfigurationSetSuppressionOptions' {} a -> s {suppressedReasons = a} :: PutConfigurationSetSuppressionOptions) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the configuration set that you want to change the
-- suppression list preferences for.
putConfigurationSetSuppressionOptions_configurationSetName :: Lens.Lens' PutConfigurationSetSuppressionOptions Prelude.Text
putConfigurationSetSuppressionOptions_configurationSetName = Lens.lens (\PutConfigurationSetSuppressionOptions' {configurationSetName} -> configurationSetName) (\s@PutConfigurationSetSuppressionOptions' {} a -> s {configurationSetName = a} :: PutConfigurationSetSuppressionOptions)

instance
  Core.AWSRequest
    PutConfigurationSetSuppressionOptions
  where
  type
    AWSResponse
      PutConfigurationSetSuppressionOptions =
      PutConfigurationSetSuppressionOptionsResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutConfigurationSetSuppressionOptionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutConfigurationSetSuppressionOptions

instance
  Prelude.NFData
    PutConfigurationSetSuppressionOptions

instance
  Core.ToHeaders
    PutConfigurationSetSuppressionOptions
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
    PutConfigurationSetSuppressionOptions
  where
  toJSON PutConfigurationSetSuppressionOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SuppressedReasons" Core..=)
              Prelude.<$> suppressedReasons
          ]
      )

instance
  Core.ToPath
    PutConfigurationSetSuppressionOptions
  where
  toPath PutConfigurationSetSuppressionOptions' {..} =
    Prelude.mconcat
      [ "/v2/email/configuration-sets/",
        Core.toBS configurationSetName,
        "/suppression-options"
      ]

instance
  Core.ToQuery
    PutConfigurationSetSuppressionOptions
  where
  toQuery = Prelude.const Prelude.mempty

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
--
-- /See:/ 'newPutConfigurationSetSuppressionOptionsResponse' smart constructor.
data PutConfigurationSetSuppressionOptionsResponse = PutConfigurationSetSuppressionOptionsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutConfigurationSetSuppressionOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putConfigurationSetSuppressionOptionsResponse_httpStatus' - The response's http status code.
newPutConfigurationSetSuppressionOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutConfigurationSetSuppressionOptionsResponse
newPutConfigurationSetSuppressionOptionsResponse
  pHttpStatus_ =
    PutConfigurationSetSuppressionOptionsResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
putConfigurationSetSuppressionOptionsResponse_httpStatus :: Lens.Lens' PutConfigurationSetSuppressionOptionsResponse Prelude.Int
putConfigurationSetSuppressionOptionsResponse_httpStatus = Lens.lens (\PutConfigurationSetSuppressionOptionsResponse' {httpStatus} -> httpStatus) (\s@PutConfigurationSetSuppressionOptionsResponse' {} a -> s {httpStatus = a} :: PutConfigurationSetSuppressionOptionsResponse)

instance
  Prelude.NFData
    PutConfigurationSetSuppressionOptionsResponse
