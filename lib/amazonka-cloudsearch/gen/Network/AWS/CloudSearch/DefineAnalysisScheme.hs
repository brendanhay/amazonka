{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DefineAnalysisScheme
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures an analysis scheme that can be applied to a @text@ or @text-array@ field to define language-specific text processing options. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-analysis-schemes.html Configuring Analysis Schemes> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DefineAnalysisScheme
  ( -- * Creating a request
    DefineAnalysisScheme (..),
    mkDefineAnalysisScheme,

    -- ** Request lenses
    dasfAnalysisScheme,
    dasfDomainName,

    -- * Destructuring the response
    DefineAnalysisSchemeResponse (..),
    mkDefineAnalysisSchemeResponse,

    -- ** Response lenses
    dasgrsAnalysisScheme,
    dasgrsResponseStatus,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'DefineAnalysisScheme' @ operation. Specifies the name of the domain you want to update and the analysis scheme configuration.
--
-- /See:/ 'mkDefineAnalysisScheme' smart constructor.
data DefineAnalysisScheme = DefineAnalysisScheme'
  { analysisScheme :: AnalysisScheme,
    domainName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DefineAnalysisScheme' with the minimum fields required to make a request.
--
-- * 'analysisScheme' -
-- * 'domainName' -
mkDefineAnalysisScheme ::
  -- | 'analysisScheme'
  AnalysisScheme ->
  -- | 'domainName'
  Lude.Text ->
  DefineAnalysisScheme
mkDefineAnalysisScheme pAnalysisScheme_ pDomainName_ =
  DefineAnalysisScheme'
    { analysisScheme = pAnalysisScheme_,
      domainName = pDomainName_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'analysisScheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasfAnalysisScheme :: Lens.Lens' DefineAnalysisScheme AnalysisScheme
dasfAnalysisScheme = Lens.lens (analysisScheme :: DefineAnalysisScheme -> AnalysisScheme) (\s a -> s {analysisScheme = a} :: DefineAnalysisScheme)
{-# DEPRECATED dasfAnalysisScheme "Use generic-lens or generic-optics with 'analysisScheme' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasfDomainName :: Lens.Lens' DefineAnalysisScheme Lude.Text
dasfDomainName = Lens.lens (domainName :: DefineAnalysisScheme -> Lude.Text) (\s a -> s {domainName = a} :: DefineAnalysisScheme)
{-# DEPRECATED dasfDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest DefineAnalysisScheme where
  type Rs DefineAnalysisScheme = DefineAnalysisSchemeResponse
  request = Req.postQuery cloudSearchService
  response =
    Res.receiveXMLWrapper
      "DefineAnalysisSchemeResult"
      ( \s h x ->
          DefineAnalysisSchemeResponse'
            Lude.<$> (x Lude..@ "AnalysisScheme")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DefineAnalysisScheme where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DefineAnalysisScheme where
  toPath = Lude.const "/"

instance Lude.ToQuery DefineAnalysisScheme where
  toQuery DefineAnalysisScheme' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DefineAnalysisScheme" :: Lude.ByteString),
        "Version" Lude.=: ("2013-01-01" :: Lude.ByteString),
        "AnalysisScheme" Lude.=: analysisScheme,
        "DomainName" Lude.=: domainName
      ]

-- | The result of a @'DefineAnalysisScheme' @ request. Contains the status of the newly-configured analysis scheme.
--
-- /See:/ 'mkDefineAnalysisSchemeResponse' smart constructor.
data DefineAnalysisSchemeResponse = DefineAnalysisSchemeResponse'
  { analysisScheme :: AnalysisSchemeStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DefineAnalysisSchemeResponse' with the minimum fields required to make a request.
--
-- * 'analysisScheme' -
-- * 'responseStatus' - The response status code.
mkDefineAnalysisSchemeResponse ::
  -- | 'analysisScheme'
  AnalysisSchemeStatus ->
  -- | 'responseStatus'
  Lude.Int ->
  DefineAnalysisSchemeResponse
mkDefineAnalysisSchemeResponse pAnalysisScheme_ pResponseStatus_ =
  DefineAnalysisSchemeResponse'
    { analysisScheme = pAnalysisScheme_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'analysisScheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasgrsAnalysisScheme :: Lens.Lens' DefineAnalysisSchemeResponse AnalysisSchemeStatus
dasgrsAnalysisScheme = Lens.lens (analysisScheme :: DefineAnalysisSchemeResponse -> AnalysisSchemeStatus) (\s a -> s {analysisScheme = a} :: DefineAnalysisSchemeResponse)
{-# DEPRECATED dasgrsAnalysisScheme "Use generic-lens or generic-optics with 'analysisScheme' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasgrsResponseStatus :: Lens.Lens' DefineAnalysisSchemeResponse Lude.Int
dasgrsResponseStatus = Lens.lens (responseStatus :: DefineAnalysisSchemeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DefineAnalysisSchemeResponse)
{-# DEPRECATED dasgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
