{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Support.DescribeSeverityLevels
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of severity levels that you can assign to an AWS
-- Support case. The severity level for a case is also a field in the
-- CaseDetails data type that you include for a CreateCase request.
--
-- -   You must have a Business or Enterprise support plan to use the AWS
--     Support API.
--
-- -   If you call the AWS Support API from an account that does not have a
--     Business or Enterprise support plan, the
--     @SubscriptionRequiredException@ error message appears. For
--     information about changing your support plan, see
--     <http://aws.amazon.com/premiumsupport/ AWS Support>.
module Network.AWS.Support.DescribeSeverityLevels
  ( -- * Creating a Request
    DescribeSeverityLevels (..),
    newDescribeSeverityLevels,

    -- * Request Lenses
    describeSeverityLevels_language,

    -- * Destructuring the Response
    DescribeSeverityLevelsResponse (..),
    newDescribeSeverityLevelsResponse,

    -- * Response Lenses
    describeSeverityLevelsResponse_severityLevels,
    describeSeverityLevelsResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Support.Types

-- | /See:/ 'newDescribeSeverityLevels' smart constructor.
data DescribeSeverityLevels = DescribeSeverityLevels'
  { -- | The ISO 639-1 code for the language in which AWS provides support. AWS
    -- Support currently supports English (\"en\") and Japanese (\"ja\").
    -- Language parameters must be passed explicitly for operations that take
    -- them.
    language :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeSeverityLevels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'language', 'describeSeverityLevels_language' - The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English (\"en\") and Japanese (\"ja\").
-- Language parameters must be passed explicitly for operations that take
-- them.
newDescribeSeverityLevels ::
  DescribeSeverityLevels
newDescribeSeverityLevels =
  DescribeSeverityLevels' {language = Prelude.Nothing}

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English (\"en\") and Japanese (\"ja\").
-- Language parameters must be passed explicitly for operations that take
-- them.
describeSeverityLevels_language :: Lens.Lens' DescribeSeverityLevels (Prelude.Maybe Prelude.Text)
describeSeverityLevels_language = Lens.lens (\DescribeSeverityLevels' {language} -> language) (\s@DescribeSeverityLevels' {} a -> s {language = a} :: DescribeSeverityLevels)

instance Prelude.AWSRequest DescribeSeverityLevels where
  type
    Rs DescribeSeverityLevels =
      DescribeSeverityLevelsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSeverityLevelsResponse'
            Prelude.<$> ( x Prelude..?> "severityLevels"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSeverityLevels

instance Prelude.NFData DescribeSeverityLevels

instance Prelude.ToHeaders DescribeSeverityLevels where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSSupport_20130415.DescribeSeverityLevels" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeSeverityLevels where
  toJSON DescribeSeverityLevels' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("language" Prelude..=) Prelude.<$> language]
      )

instance Prelude.ToPath DescribeSeverityLevels where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeSeverityLevels where
  toQuery = Prelude.const Prelude.mempty

-- | The list of severity levels returned by the DescribeSeverityLevels
-- operation.
--
-- /See:/ 'newDescribeSeverityLevelsResponse' smart constructor.
data DescribeSeverityLevelsResponse = DescribeSeverityLevelsResponse'
  { -- | The available severity levels for the support case. Available severity
    -- levels are defined by your service level agreement with AWS.
    severityLevels :: Prelude.Maybe [SeverityLevel],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeSeverityLevelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'severityLevels', 'describeSeverityLevelsResponse_severityLevels' - The available severity levels for the support case. Available severity
-- levels are defined by your service level agreement with AWS.
--
-- 'httpStatus', 'describeSeverityLevelsResponse_httpStatus' - The response's http status code.
newDescribeSeverityLevelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSeverityLevelsResponse
newDescribeSeverityLevelsResponse pHttpStatus_ =
  DescribeSeverityLevelsResponse'
    { severityLevels =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The available severity levels for the support case. Available severity
-- levels are defined by your service level agreement with AWS.
describeSeverityLevelsResponse_severityLevels :: Lens.Lens' DescribeSeverityLevelsResponse (Prelude.Maybe [SeverityLevel])
describeSeverityLevelsResponse_severityLevels = Lens.lens (\DescribeSeverityLevelsResponse' {severityLevels} -> severityLevels) (\s@DescribeSeverityLevelsResponse' {} a -> s {severityLevels = a} :: DescribeSeverityLevelsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeSeverityLevelsResponse_httpStatus :: Lens.Lens' DescribeSeverityLevelsResponse Prelude.Int
describeSeverityLevelsResponse_httpStatus = Lens.lens (\DescribeSeverityLevelsResponse' {httpStatus} -> httpStatus) (\s@DescribeSeverityLevelsResponse' {} a -> s {httpStatus = a} :: DescribeSeverityLevelsResponse)

instance
  Prelude.NFData
    DescribeSeverityLevelsResponse
