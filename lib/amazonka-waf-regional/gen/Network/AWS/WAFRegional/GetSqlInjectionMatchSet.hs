{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.GetSqlInjectionMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'SqlInjectionMatchSet' that is specified by @SqlInjectionMatchSetId@ .
module Network.AWS.WAFRegional.GetSqlInjectionMatchSet
  ( -- * Creating a request
    GetSqlInjectionMatchSet (..),
    mkGetSqlInjectionMatchSet,

    -- ** Request lenses
    gsimsSqlInjectionMatchSetId,

    -- * Destructuring the response
    GetSqlInjectionMatchSetResponse (..),
    mkGetSqlInjectionMatchSetResponse,

    -- ** Response lenses
    gsimsrsSqlInjectionMatchSet,
    gsimsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | A request to get a 'SqlInjectionMatchSet' .
--
-- /See:/ 'mkGetSqlInjectionMatchSet' smart constructor.
newtype GetSqlInjectionMatchSet = GetSqlInjectionMatchSet'
  { -- | The @SqlInjectionMatchSetId@ of the 'SqlInjectionMatchSet' that you want to get. @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
    sqlInjectionMatchSetId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSqlInjectionMatchSet' with the minimum fields required to make a request.
--
-- * 'sqlInjectionMatchSetId' - The @SqlInjectionMatchSetId@ of the 'SqlInjectionMatchSet' that you want to get. @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
mkGetSqlInjectionMatchSet ::
  -- | 'sqlInjectionMatchSetId'
  Lude.Text ->
  GetSqlInjectionMatchSet
mkGetSqlInjectionMatchSet pSqlInjectionMatchSetId_ =
  GetSqlInjectionMatchSet'
    { sqlInjectionMatchSetId =
        pSqlInjectionMatchSetId_
    }

-- | The @SqlInjectionMatchSetId@ of the 'SqlInjectionMatchSet' that you want to get. @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
--
-- /Note:/ Consider using 'sqlInjectionMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsimsSqlInjectionMatchSetId :: Lens.Lens' GetSqlInjectionMatchSet Lude.Text
gsimsSqlInjectionMatchSetId = Lens.lens (sqlInjectionMatchSetId :: GetSqlInjectionMatchSet -> Lude.Text) (\s a -> s {sqlInjectionMatchSetId = a} :: GetSqlInjectionMatchSet)
{-# DEPRECATED gsimsSqlInjectionMatchSetId "Use generic-lens or generic-optics with 'sqlInjectionMatchSetId' instead." #-}

instance Lude.AWSRequest GetSqlInjectionMatchSet where
  type Rs GetSqlInjectionMatchSet = GetSqlInjectionMatchSetResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSqlInjectionMatchSetResponse'
            Lude.<$> (x Lude..?> "SqlInjectionMatchSet")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSqlInjectionMatchSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSWAF_Regional_20161128.GetSqlInjectionMatchSet" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetSqlInjectionMatchSet where
  toJSON GetSqlInjectionMatchSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("SqlInjectionMatchSetId" Lude..= sqlInjectionMatchSetId)
          ]
      )

instance Lude.ToPath GetSqlInjectionMatchSet where
  toPath = Lude.const "/"

instance Lude.ToQuery GetSqlInjectionMatchSet where
  toQuery = Lude.const Lude.mempty

-- | The response to a 'GetSqlInjectionMatchSet' request.
--
-- /See:/ 'mkGetSqlInjectionMatchSetResponse' smart constructor.
data GetSqlInjectionMatchSetResponse = GetSqlInjectionMatchSetResponse'
  { -- | Information about the 'SqlInjectionMatchSet' that you specified in the @GetSqlInjectionMatchSet@ request. For more information, see the following topics:
    --
    --
    --     * 'SqlInjectionMatchSet' : Contains @Name@ , @SqlInjectionMatchSetId@ , and an array of @SqlInjectionMatchTuple@ objects
    --
    --
    --     * 'SqlInjectionMatchTuple' : Each @SqlInjectionMatchTuple@ object contains @FieldToMatch@ and @TextTransformation@
    --
    --
    --     * 'FieldToMatch' : Contains @Data@ and @Type@
    sqlInjectionMatchSet :: Lude.Maybe SqlInjectionMatchSet,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSqlInjectionMatchSetResponse' with the minimum fields required to make a request.
--
-- * 'sqlInjectionMatchSet' - Information about the 'SqlInjectionMatchSet' that you specified in the @GetSqlInjectionMatchSet@ request. For more information, see the following topics:
--
--
--     * 'SqlInjectionMatchSet' : Contains @Name@ , @SqlInjectionMatchSetId@ , and an array of @SqlInjectionMatchTuple@ objects
--
--
--     * 'SqlInjectionMatchTuple' : Each @SqlInjectionMatchTuple@ object contains @FieldToMatch@ and @TextTransformation@
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@
--
--
-- * 'responseStatus' - The response status code.
mkGetSqlInjectionMatchSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSqlInjectionMatchSetResponse
mkGetSqlInjectionMatchSetResponse pResponseStatus_ =
  GetSqlInjectionMatchSetResponse'
    { sqlInjectionMatchSet =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the 'SqlInjectionMatchSet' that you specified in the @GetSqlInjectionMatchSet@ request. For more information, see the following topics:
--
--
--     * 'SqlInjectionMatchSet' : Contains @Name@ , @SqlInjectionMatchSetId@ , and an array of @SqlInjectionMatchTuple@ objects
--
--
--     * 'SqlInjectionMatchTuple' : Each @SqlInjectionMatchTuple@ object contains @FieldToMatch@ and @TextTransformation@
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@
--
--
--
-- /Note:/ Consider using 'sqlInjectionMatchSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsimsrsSqlInjectionMatchSet :: Lens.Lens' GetSqlInjectionMatchSetResponse (Lude.Maybe SqlInjectionMatchSet)
gsimsrsSqlInjectionMatchSet = Lens.lens (sqlInjectionMatchSet :: GetSqlInjectionMatchSetResponse -> Lude.Maybe SqlInjectionMatchSet) (\s a -> s {sqlInjectionMatchSet = a} :: GetSqlInjectionMatchSetResponse)
{-# DEPRECATED gsimsrsSqlInjectionMatchSet "Use generic-lens or generic-optics with 'sqlInjectionMatchSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsimsrsResponseStatus :: Lens.Lens' GetSqlInjectionMatchSetResponse Lude.Int
gsimsrsResponseStatus = Lens.lens (responseStatus :: GetSqlInjectionMatchSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSqlInjectionMatchSetResponse)
{-# DEPRECATED gsimsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
