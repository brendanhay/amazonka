{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.UpdateGeoMatchSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes 'GeoMatchConstraint' objects in an @GeoMatchSet@ . For each @GeoMatchConstraint@ object, you specify the following values:
--
--
--     * Whether to insert or delete the object from the array. If you want to change an @GeoMatchConstraint@ object, you delete the existing object and add a new one.
--
--     * The @Type@ . The only valid value for @Type@ is @Country@ .
--
--     * The @Value@ , which is a two character code for the country to add to the @GeoMatchConstraint@ object. Valid codes are listed in 'GeoMatchConstraint$Value' .
--
--
--
-- To create and configure an @GeoMatchSet@ , perform the following steps:
--
--     * Submit a 'CreateGeoMatchSet' request.
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateGeoMatchSet' request.
--
--     * Submit an @UpdateGeoMatchSet@ request to specify the country that you want AWS WAF to watch for.
--
--
--
-- When you update an @GeoMatchSet@ , you specify the country that you want to add and/or the country that you want to delete. If you want to change a country, you delete the existing country and add the new one.
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <http://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
--
module Network.AWS.WAF.UpdateGeoMatchSet
    (
    -- * Creating a Request
      updateGeoMatchSet
    , UpdateGeoMatchSet
    -- * Request Lenses
    , ugmsGeoMatchSetId
    , ugmsChangeToken
    , ugmsUpdates

    -- * Destructuring the Response
    , updateGeoMatchSetResponse
    , UpdateGeoMatchSetResponse
    -- * Response Lenses
    , ugmsrsChangeToken
    , ugmsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAF.Types
import Network.AWS.WAF.Types.Product

-- | /See:/ 'updateGeoMatchSet' smart constructor.
data UpdateGeoMatchSet = UpdateGeoMatchSet'
  { _ugmsGeoMatchSetId :: !Text
  , _ugmsChangeToken   :: !Text
  , _ugmsUpdates       :: !(List1 GeoMatchSetUpdate)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateGeoMatchSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugmsGeoMatchSetId' - The @GeoMatchSetId@ of the 'GeoMatchSet' that you want to update. @GeoMatchSetId@ is returned by 'CreateGeoMatchSet' and by 'ListGeoMatchSets' .
--
-- * 'ugmsChangeToken' - The value returned by the most recent call to 'GetChangeToken' .
--
-- * 'ugmsUpdates' - An array of @GeoMatchSetUpdate@ objects that you want to insert into or delete from an 'GeoMatchSet' . For more information, see the applicable data types:     * 'GeoMatchSetUpdate' : Contains @Action@ and @GeoMatchConstraint@      * 'GeoMatchConstraint' : Contains @Type@ and @Value@  You can have only one @Type@ and @Value@ per @GeoMatchConstraint@ . To add multiple countries, include multiple @GeoMatchSetUpdate@ objects in your request.
updateGeoMatchSet
    :: Text -- ^ 'ugmsGeoMatchSetId'
    -> Text -- ^ 'ugmsChangeToken'
    -> NonEmpty GeoMatchSetUpdate -- ^ 'ugmsUpdates'
    -> UpdateGeoMatchSet
updateGeoMatchSet pGeoMatchSetId_ pChangeToken_ pUpdates_ =
  UpdateGeoMatchSet'
    { _ugmsGeoMatchSetId = pGeoMatchSetId_
    , _ugmsChangeToken = pChangeToken_
    , _ugmsUpdates = _List1 # pUpdates_
    }


-- | The @GeoMatchSetId@ of the 'GeoMatchSet' that you want to update. @GeoMatchSetId@ is returned by 'CreateGeoMatchSet' and by 'ListGeoMatchSets' .
ugmsGeoMatchSetId :: Lens' UpdateGeoMatchSet Text
ugmsGeoMatchSetId = lens _ugmsGeoMatchSetId (\ s a -> s{_ugmsGeoMatchSetId = a})

-- | The value returned by the most recent call to 'GetChangeToken' .
ugmsChangeToken :: Lens' UpdateGeoMatchSet Text
ugmsChangeToken = lens _ugmsChangeToken (\ s a -> s{_ugmsChangeToken = a})

-- | An array of @GeoMatchSetUpdate@ objects that you want to insert into or delete from an 'GeoMatchSet' . For more information, see the applicable data types:     * 'GeoMatchSetUpdate' : Contains @Action@ and @GeoMatchConstraint@      * 'GeoMatchConstraint' : Contains @Type@ and @Value@  You can have only one @Type@ and @Value@ per @GeoMatchConstraint@ . To add multiple countries, include multiple @GeoMatchSetUpdate@ objects in your request.
ugmsUpdates :: Lens' UpdateGeoMatchSet (NonEmpty GeoMatchSetUpdate)
ugmsUpdates = lens _ugmsUpdates (\ s a -> s{_ugmsUpdates = a}) . _List1

instance AWSRequest UpdateGeoMatchSet where
        type Rs UpdateGeoMatchSet = UpdateGeoMatchSetResponse
        request = postJSON waf
        response
          = receiveJSON
              (\ s h x ->
                 UpdateGeoMatchSetResponse' <$>
                   (x .?> "ChangeToken") <*> (pure (fromEnum s)))

instance Hashable UpdateGeoMatchSet where

instance NFData UpdateGeoMatchSet where

instance ToHeaders UpdateGeoMatchSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_20150824.UpdateGeoMatchSet" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateGeoMatchSet where
        toJSON UpdateGeoMatchSet'{..}
          = object
              (catMaybes
                 [Just ("GeoMatchSetId" .= _ugmsGeoMatchSetId),
                  Just ("ChangeToken" .= _ugmsChangeToken),
                  Just ("Updates" .= _ugmsUpdates)])

instance ToPath UpdateGeoMatchSet where
        toPath = const "/"

instance ToQuery UpdateGeoMatchSet where
        toQuery = const mempty

-- | /See:/ 'updateGeoMatchSetResponse' smart constructor.
data UpdateGeoMatchSetResponse = UpdateGeoMatchSetResponse'
  { _ugmsrsChangeToken    :: !(Maybe Text)
  , _ugmsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateGeoMatchSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugmsrsChangeToken' - The @ChangeToken@ that you used to submit the @UpdateGeoMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- * 'ugmsrsResponseStatus' - -- | The response status code.
updateGeoMatchSetResponse
    :: Int -- ^ 'ugmsrsResponseStatus'
    -> UpdateGeoMatchSetResponse
updateGeoMatchSetResponse pResponseStatus_ =
  UpdateGeoMatchSetResponse'
    {_ugmsrsChangeToken = Nothing, _ugmsrsResponseStatus = pResponseStatus_}


-- | The @ChangeToken@ that you used to submit the @UpdateGeoMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
ugmsrsChangeToken :: Lens' UpdateGeoMatchSetResponse (Maybe Text)
ugmsrsChangeToken = lens _ugmsrsChangeToken (\ s a -> s{_ugmsrsChangeToken = a})

-- | -- | The response status code.
ugmsrsResponseStatus :: Lens' UpdateGeoMatchSetResponse Int
ugmsrsResponseStatus = lens _ugmsrsResponseStatus (\ s a -> s{_ugmsrsResponseStatus = a})

instance NFData UpdateGeoMatchSetResponse where
