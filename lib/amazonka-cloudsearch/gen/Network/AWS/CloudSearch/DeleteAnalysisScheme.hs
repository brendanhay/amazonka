{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DeleteAnalysisScheme
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an analysis scheme. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-analysis-schemes.html Configuring Analysis Schemes> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DeleteAnalysisScheme
  ( -- * Creating a request
    DeleteAnalysisScheme (..),
    mkDeleteAnalysisScheme,

    -- ** Request lenses
    dasDomainName,
    dasAnalysisSchemeName,

    -- * Destructuring the response
    DeleteAnalysisSchemeResponse (..),
    mkDeleteAnalysisSchemeResponse,

    -- ** Response lenses
    dasfrsAnalysisScheme,
    dasfrsResponseStatus,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'DeleteAnalysisScheme' @ operation. Specifies the name of the domain you want to update and the analysis scheme you want to delete.
--
-- /See:/ 'mkDeleteAnalysisScheme' smart constructor.
data DeleteAnalysisScheme = DeleteAnalysisScheme'
  { domainName :: Lude.Text,
    -- | The name of the analysis scheme you want to delete.
    analysisSchemeName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAnalysisScheme' with the minimum fields required to make a request.
--
-- * 'domainName' -
-- * 'analysisSchemeName' - The name of the analysis scheme you want to delete.
mkDeleteAnalysisScheme ::
  -- | 'domainName'
  Lude.Text ->
  -- | 'analysisSchemeName'
  Lude.Text ->
  DeleteAnalysisScheme
mkDeleteAnalysisScheme pDomainName_ pAnalysisSchemeName_ =
  DeleteAnalysisScheme'
    { domainName = pDomainName_,
      analysisSchemeName = pAnalysisSchemeName_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasDomainName :: Lens.Lens' DeleteAnalysisScheme Lude.Text
dasDomainName = Lens.lens (domainName :: DeleteAnalysisScheme -> Lude.Text) (\s a -> s {domainName = a} :: DeleteAnalysisScheme)
{-# DEPRECATED dasDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The name of the analysis scheme you want to delete.
--
-- /Note:/ Consider using 'analysisSchemeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasAnalysisSchemeName :: Lens.Lens' DeleteAnalysisScheme Lude.Text
dasAnalysisSchemeName = Lens.lens (analysisSchemeName :: DeleteAnalysisScheme -> Lude.Text) (\s a -> s {analysisSchemeName = a} :: DeleteAnalysisScheme)
{-# DEPRECATED dasAnalysisSchemeName "Use generic-lens or generic-optics with 'analysisSchemeName' instead." #-}

instance Lude.AWSRequest DeleteAnalysisScheme where
  type Rs DeleteAnalysisScheme = DeleteAnalysisSchemeResponse
  request = Req.postQuery cloudSearchService
  response =
    Res.receiveXMLWrapper
      "DeleteAnalysisSchemeResult"
      ( \s h x ->
          DeleteAnalysisSchemeResponse'
            Lude.<$> (x Lude..@ "AnalysisScheme")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteAnalysisScheme where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteAnalysisScheme where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteAnalysisScheme where
  toQuery DeleteAnalysisScheme' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteAnalysisScheme" :: Lude.ByteString),
        "Version" Lude.=: ("2013-01-01" :: Lude.ByteString),
        "DomainName" Lude.=: domainName,
        "AnalysisSchemeName" Lude.=: analysisSchemeName
      ]

-- | The result of a @DeleteAnalysisScheme@ request. Contains the status of the deleted analysis scheme.
--
-- /See:/ 'mkDeleteAnalysisSchemeResponse' smart constructor.
data DeleteAnalysisSchemeResponse = DeleteAnalysisSchemeResponse'
  { -- | The status of the analysis scheme being deleted.
    analysisScheme :: AnalysisSchemeStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAnalysisSchemeResponse' with the minimum fields required to make a request.
--
-- * 'analysisScheme' - The status of the analysis scheme being deleted.
-- * 'responseStatus' - The response status code.
mkDeleteAnalysisSchemeResponse ::
  -- | 'analysisScheme'
  AnalysisSchemeStatus ->
  -- | 'responseStatus'
  Lude.Int ->
  DeleteAnalysisSchemeResponse
mkDeleteAnalysisSchemeResponse pAnalysisScheme_ pResponseStatus_ =
  DeleteAnalysisSchemeResponse'
    { analysisScheme = pAnalysisScheme_,
      responseStatus = pResponseStatus_
    }

-- | The status of the analysis scheme being deleted.
--
-- /Note:/ Consider using 'analysisScheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasfrsAnalysisScheme :: Lens.Lens' DeleteAnalysisSchemeResponse AnalysisSchemeStatus
dasfrsAnalysisScheme = Lens.lens (analysisScheme :: DeleteAnalysisSchemeResponse -> AnalysisSchemeStatus) (\s a -> s {analysisScheme = a} :: DeleteAnalysisSchemeResponse)
{-# DEPRECATED dasfrsAnalysisScheme "Use generic-lens or generic-optics with 'analysisScheme' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasfrsResponseStatus :: Lens.Lens' DeleteAnalysisSchemeResponse Lude.Int
dasfrsResponseStatus = Lens.lens (responseStatus :: DeleteAnalysisSchemeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteAnalysisSchemeResponse)
{-# DEPRECATED dasfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
