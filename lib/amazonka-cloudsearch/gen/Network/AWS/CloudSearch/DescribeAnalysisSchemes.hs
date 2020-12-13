{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DescribeAnalysisSchemes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the analysis schemes configured for a domain. An analysis scheme defines language-specific text processing options for a @text@ field. Can be limited to specific analysis schemes by name. By default, shows all analysis schemes and includes any pending changes to the configuration. Set the @Deployed@ option to @true@ to show the active configuration and exclude pending changes. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-analysis-schemes.html Configuring Analysis Schemes> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DescribeAnalysisSchemes
  ( -- * Creating a request
    DescribeAnalysisSchemes (..),
    mkDescribeAnalysisSchemes,

    -- ** Request lenses
    dassDeployed,
    dassAnalysisSchemeNames,
    dassDomainName,

    -- * Destructuring the response
    DescribeAnalysisSchemesResponse (..),
    mkDescribeAnalysisSchemesResponse,

    -- ** Response lenses
    dasrsAnalysisSchemes,
    dasrsResponseStatus,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'DescribeAnalysisSchemes' @ operation. Specifies the name of the domain you want to describe. To limit the response to particular analysis schemes, specify the names of the analysis schemes you want to describe. To show the active configuration and exclude any pending changes, set the @Deployed@ option to @true@ .
--
-- /See:/ 'mkDescribeAnalysisSchemes' smart constructor.
data DescribeAnalysisSchemes = DescribeAnalysisSchemes'
  { -- | Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
    deployed :: Lude.Maybe Lude.Bool,
    -- | The analysis schemes you want to describe.
    analysisSchemeNames :: Lude.Maybe [Lude.Text],
    -- | The name of the domain you want to describe.
    domainName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAnalysisSchemes' with the minimum fields required to make a request.
--
-- * 'deployed' - Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
-- * 'analysisSchemeNames' - The analysis schemes you want to describe.
-- * 'domainName' - The name of the domain you want to describe.
mkDescribeAnalysisSchemes ::
  -- | 'domainName'
  Lude.Text ->
  DescribeAnalysisSchemes
mkDescribeAnalysisSchemes pDomainName_ =
  DescribeAnalysisSchemes'
    { deployed = Lude.Nothing,
      analysisSchemeNames = Lude.Nothing,
      domainName = pDomainName_
    }

-- | Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
--
-- /Note:/ Consider using 'deployed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dassDeployed :: Lens.Lens' DescribeAnalysisSchemes (Lude.Maybe Lude.Bool)
dassDeployed = Lens.lens (deployed :: DescribeAnalysisSchemes -> Lude.Maybe Lude.Bool) (\s a -> s {deployed = a} :: DescribeAnalysisSchemes)
{-# DEPRECATED dassDeployed "Use generic-lens or generic-optics with 'deployed' instead." #-}

-- | The analysis schemes you want to describe.
--
-- /Note:/ Consider using 'analysisSchemeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dassAnalysisSchemeNames :: Lens.Lens' DescribeAnalysisSchemes (Lude.Maybe [Lude.Text])
dassAnalysisSchemeNames = Lens.lens (analysisSchemeNames :: DescribeAnalysisSchemes -> Lude.Maybe [Lude.Text]) (\s a -> s {analysisSchemeNames = a} :: DescribeAnalysisSchemes)
{-# DEPRECATED dassAnalysisSchemeNames "Use generic-lens or generic-optics with 'analysisSchemeNames' instead." #-}

-- | The name of the domain you want to describe.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dassDomainName :: Lens.Lens' DescribeAnalysisSchemes Lude.Text
dassDomainName = Lens.lens (domainName :: DescribeAnalysisSchemes -> Lude.Text) (\s a -> s {domainName = a} :: DescribeAnalysisSchemes)
{-# DEPRECATED dassDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest DescribeAnalysisSchemes where
  type Rs DescribeAnalysisSchemes = DescribeAnalysisSchemesResponse
  request = Req.postQuery cloudSearchService
  response =
    Res.receiveXMLWrapper
      "DescribeAnalysisSchemesResult"
      ( \s h x ->
          DescribeAnalysisSchemesResponse'
            Lude.<$> ( x Lude..@? "AnalysisSchemes" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "member"
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAnalysisSchemes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeAnalysisSchemes where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAnalysisSchemes where
  toQuery DescribeAnalysisSchemes' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeAnalysisSchemes" :: Lude.ByteString),
        "Version" Lude.=: ("2013-01-01" :: Lude.ByteString),
        "Deployed" Lude.=: deployed,
        "AnalysisSchemeNames"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> analysisSchemeNames),
        "DomainName" Lude.=: domainName
      ]

-- | The result of a @DescribeAnalysisSchemes@ request. Contains the analysis schemes configured for the domain specified in the request.
--
-- /See:/ 'mkDescribeAnalysisSchemesResponse' smart constructor.
data DescribeAnalysisSchemesResponse = DescribeAnalysisSchemesResponse'
  { -- | The analysis scheme descriptions.
    analysisSchemes :: [AnalysisSchemeStatus],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAnalysisSchemesResponse' with the minimum fields required to make a request.
--
-- * 'analysisSchemes' - The analysis scheme descriptions.
-- * 'responseStatus' - The response status code.
mkDescribeAnalysisSchemesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAnalysisSchemesResponse
mkDescribeAnalysisSchemesResponse pResponseStatus_ =
  DescribeAnalysisSchemesResponse'
    { analysisSchemes = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | The analysis scheme descriptions.
--
-- /Note:/ Consider using 'analysisSchemes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrsAnalysisSchemes :: Lens.Lens' DescribeAnalysisSchemesResponse [AnalysisSchemeStatus]
dasrsAnalysisSchemes = Lens.lens (analysisSchemes :: DescribeAnalysisSchemesResponse -> [AnalysisSchemeStatus]) (\s a -> s {analysisSchemes = a} :: DescribeAnalysisSchemesResponse)
{-# DEPRECATED dasrsAnalysisSchemes "Use generic-lens or generic-optics with 'analysisSchemes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrsResponseStatus :: Lens.Lens' DescribeAnalysisSchemesResponse Lude.Int
dasrsResponseStatus = Lens.lens (responseStatus :: DescribeAnalysisSchemesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAnalysisSchemesResponse)
{-# DEPRECATED dasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
