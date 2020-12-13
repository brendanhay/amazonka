{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.FieldToMatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.FieldToMatch
  ( FieldToMatch (..),

    -- * Smart constructor
    mkFieldToMatch,

    -- * Lenses
    ftmData,
    ftmType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAFRegional.Types.MatchFieldType

-- | Specifies where in a web request to look for @TargetString@ .
--
-- /See:/ 'mkFieldToMatch' smart constructor.
data FieldToMatch = FieldToMatch'
  { -- | When the value of @Type@ is @HEADER@ , enter the name of the header that you want AWS WAF to search, for example, @User-Agent@ or @Referer@ . The name of the header is not case sensitive.
    --
    -- When the value of @Type@ is @SINGLE_QUERY_ARG@ , enter the name of the parameter that you want AWS WAF to search, for example, @UserName@ or @SalesRegion@ . The parameter name is not case sensitive.
    -- If the value of @Type@ is any other value, omit @Data@ .
    data' :: Lude.Maybe Lude.Text,
    -- | The part of the web request that you want AWS WAF to search for a specified string. Parts of a request that you can search include the following:
    --
    --
    --     * @HEADER@ : A specified request header, for example, the value of the @User-Agent@ or @Referer@ header. If you choose @HEADER@ for the type, specify the name of the header in @Data@ .
    --
    --
    --     * @METHOD@ : The HTTP method, which indicated the type of operation that the request is asking the origin to perform. Amazon CloudFront supports the following methods: @DELETE@ , @GET@ , @HEAD@ , @OPTIONS@ , @PATCH@ , @POST@ , and @PUT@ .
    --
    --
    --     * @QUERY_STRING@ : A query string, which is the part of a URL that appears after a @?@ character, if any.
    --
    --
    --     * @URI@ : The part of a web request that identifies a resource, for example, @/images/daily-ad.jpg@ .
    --
    --
    --     * @BODY@ : The part of a request that contains any additional data that you want to send to your web server as the HTTP request body, such as data from a form. The request body immediately follows the request headers. Note that only the first @8192@ bytes of the request body are forwarded to AWS WAF for inspection. To allow or block requests based on the length of the body, you can create a size constraint set. For more information, see 'CreateSizeConstraintSet' .
    --
    --
    --     * @SINGLE_QUERY_ARG@ : The parameter in the query string that you will inspect, such as /UserName/ or /SalesRegion/ . The maximum length for @SINGLE_QUERY_ARG@ is 30 characters.
    --
    --
    --     * @ALL_QUERY_ARGS@ : Similar to @SINGLE_QUERY_ARG@ , but rather than inspecting a single parameter, AWS WAF will inspect all parameters within the query for the value or regex pattern that you specify in @TargetString@ .
    type' :: MatchFieldType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FieldToMatch' with the minimum fields required to make a request.
--
-- * 'data'' - When the value of @Type@ is @HEADER@ , enter the name of the header that you want AWS WAF to search, for example, @User-Agent@ or @Referer@ . The name of the header is not case sensitive.
--
-- When the value of @Type@ is @SINGLE_QUERY_ARG@ , enter the name of the parameter that you want AWS WAF to search, for example, @UserName@ or @SalesRegion@ . The parameter name is not case sensitive.
-- If the value of @Type@ is any other value, omit @Data@ .
-- * 'type'' - The part of the web request that you want AWS WAF to search for a specified string. Parts of a request that you can search include the following:
--
--
--     * @HEADER@ : A specified request header, for example, the value of the @User-Agent@ or @Referer@ header. If you choose @HEADER@ for the type, specify the name of the header in @Data@ .
--
--
--     * @METHOD@ : The HTTP method, which indicated the type of operation that the request is asking the origin to perform. Amazon CloudFront supports the following methods: @DELETE@ , @GET@ , @HEAD@ , @OPTIONS@ , @PATCH@ , @POST@ , and @PUT@ .
--
--
--     * @QUERY_STRING@ : A query string, which is the part of a URL that appears after a @?@ character, if any.
--
--
--     * @URI@ : The part of a web request that identifies a resource, for example, @/images/daily-ad.jpg@ .
--
--
--     * @BODY@ : The part of a request that contains any additional data that you want to send to your web server as the HTTP request body, such as data from a form. The request body immediately follows the request headers. Note that only the first @8192@ bytes of the request body are forwarded to AWS WAF for inspection. To allow or block requests based on the length of the body, you can create a size constraint set. For more information, see 'CreateSizeConstraintSet' .
--
--
--     * @SINGLE_QUERY_ARG@ : The parameter in the query string that you will inspect, such as /UserName/ or /SalesRegion/ . The maximum length for @SINGLE_QUERY_ARG@ is 30 characters.
--
--
--     * @ALL_QUERY_ARGS@ : Similar to @SINGLE_QUERY_ARG@ , but rather than inspecting a single parameter, AWS WAF will inspect all parameters within the query for the value or regex pattern that you specify in @TargetString@ .
mkFieldToMatch ::
  -- | 'type''
  MatchFieldType ->
  FieldToMatch
mkFieldToMatch pType_ =
  FieldToMatch' {data' = Lude.Nothing, type' = pType_}

-- | When the value of @Type@ is @HEADER@ , enter the name of the header that you want AWS WAF to search, for example, @User-Agent@ or @Referer@ . The name of the header is not case sensitive.
--
-- When the value of @Type@ is @SINGLE_QUERY_ARG@ , enter the name of the parameter that you want AWS WAF to search, for example, @UserName@ or @SalesRegion@ . The parameter name is not case sensitive.
-- If the value of @Type@ is any other value, omit @Data@ .
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ftmData :: Lens.Lens' FieldToMatch (Lude.Maybe Lude.Text)
ftmData = Lens.lens (data' :: FieldToMatch -> Lude.Maybe Lude.Text) (\s a -> s {data' = a} :: FieldToMatch)
{-# DEPRECATED ftmData "Use generic-lens or generic-optics with 'data'' instead." #-}

-- | The part of the web request that you want AWS WAF to search for a specified string. Parts of a request that you can search include the following:
--
--
--     * @HEADER@ : A specified request header, for example, the value of the @User-Agent@ or @Referer@ header. If you choose @HEADER@ for the type, specify the name of the header in @Data@ .
--
--
--     * @METHOD@ : The HTTP method, which indicated the type of operation that the request is asking the origin to perform. Amazon CloudFront supports the following methods: @DELETE@ , @GET@ , @HEAD@ , @OPTIONS@ , @PATCH@ , @POST@ , and @PUT@ .
--
--
--     * @QUERY_STRING@ : A query string, which is the part of a URL that appears after a @?@ character, if any.
--
--
--     * @URI@ : The part of a web request that identifies a resource, for example, @/images/daily-ad.jpg@ .
--
--
--     * @BODY@ : The part of a request that contains any additional data that you want to send to your web server as the HTTP request body, such as data from a form. The request body immediately follows the request headers. Note that only the first @8192@ bytes of the request body are forwarded to AWS WAF for inspection. To allow or block requests based on the length of the body, you can create a size constraint set. For more information, see 'CreateSizeConstraintSet' .
--
--
--     * @SINGLE_QUERY_ARG@ : The parameter in the query string that you will inspect, such as /UserName/ or /SalesRegion/ . The maximum length for @SINGLE_QUERY_ARG@ is 30 characters.
--
--
--     * @ALL_QUERY_ARGS@ : Similar to @SINGLE_QUERY_ARG@ , but rather than inspecting a single parameter, AWS WAF will inspect all parameters within the query for the value or regex pattern that you specify in @TargetString@ .
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ftmType :: Lens.Lens' FieldToMatch MatchFieldType
ftmType = Lens.lens (type' :: FieldToMatch -> MatchFieldType) (\s a -> s {type' = a} :: FieldToMatch)
{-# DEPRECATED ftmType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON FieldToMatch where
  parseJSON =
    Lude.withObject
      "FieldToMatch"
      ( \x ->
          FieldToMatch'
            Lude.<$> (x Lude..:? "Data") Lude.<*> (x Lude..: "Type")
      )

instance Lude.ToJSON FieldToMatch where
  toJSON FieldToMatch' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Data" Lude..=) Lude.<$> data',
            Lude.Just ("Type" Lude..= type')
          ]
      )
