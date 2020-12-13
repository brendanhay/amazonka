{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GenerateCredentialReport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a credential report for the AWS account. For more information about the credential report, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html Getting Credential Reports> in the /IAM User Guide/ .
module Network.AWS.IAM.GenerateCredentialReport
  ( -- * Creating a request
    GenerateCredentialReport (..),
    mkGenerateCredentialReport,

    -- * Destructuring the response
    GenerateCredentialReportResponse (..),
    mkGenerateCredentialReportResponse,

    -- ** Response lenses
    gcrrsState,
    gcrrsDescription,
    gcrrsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGenerateCredentialReport' smart constructor.
data GenerateCredentialReport = GenerateCredentialReport'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GenerateCredentialReport' with the minimum fields required to make a request.
mkGenerateCredentialReport ::
  GenerateCredentialReport
mkGenerateCredentialReport = GenerateCredentialReport'

instance Lude.AWSRequest GenerateCredentialReport where
  type Rs GenerateCredentialReport = GenerateCredentialReportResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "GenerateCredentialReportResult"
      ( \s h x ->
          GenerateCredentialReportResponse'
            Lude.<$> (x Lude..@? "State")
            Lude.<*> (x Lude..@? "Description")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GenerateCredentialReport where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GenerateCredentialReport where
  toPath = Lude.const "/"

instance Lude.ToQuery GenerateCredentialReport where
  toQuery =
    Lude.const
      ( Lude.mconcat
          [ "Action" Lude.=: ("GenerateCredentialReport" :: Lude.ByteString),
            "Version" Lude.=: ("2010-05-08" :: Lude.ByteString)
          ]
      )

-- | Contains the response to a successful 'GenerateCredentialReport' request.
--
-- /See:/ 'mkGenerateCredentialReportResponse' smart constructor.
data GenerateCredentialReportResponse = GenerateCredentialReportResponse'
  { -- | Information about the state of the credential report.
    state :: Lude.Maybe ReportStateType,
    -- | Information about the credential report.
    description :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GenerateCredentialReportResponse' with the minimum fields required to make a request.
--
-- * 'state' - Information about the state of the credential report.
-- * 'description' - Information about the credential report.
-- * 'responseStatus' - The response status code.
mkGenerateCredentialReportResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GenerateCredentialReportResponse
mkGenerateCredentialReportResponse pResponseStatus_ =
  GenerateCredentialReportResponse'
    { state = Lude.Nothing,
      description = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the state of the credential report.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsState :: Lens.Lens' GenerateCredentialReportResponse (Lude.Maybe ReportStateType)
gcrrsState = Lens.lens (state :: GenerateCredentialReportResponse -> Lude.Maybe ReportStateType) (\s a -> s {state = a} :: GenerateCredentialReportResponse)
{-# DEPRECATED gcrrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Information about the credential report.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsDescription :: Lens.Lens' GenerateCredentialReportResponse (Lude.Maybe Lude.Text)
gcrrsDescription = Lens.lens (description :: GenerateCredentialReportResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: GenerateCredentialReportResponse)
{-# DEPRECATED gcrrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsResponseStatus :: Lens.Lens' GenerateCredentialReportResponse Lude.Int
gcrrsResponseStatus = Lens.lens (responseStatus :: GenerateCredentialReportResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GenerateCredentialReportResponse)
{-# DEPRECATED gcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
