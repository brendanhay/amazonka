{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexRuntime.Types.Product where

import           Network.AWS.Lens
import           Network.AWS.LexRuntime.Types.Sum
import           Network.AWS.Prelude

-- | Represents an option to be shown on the client platform (Facebook, Slack, etc.)
--
--
--
-- /See:/ 'button' smart constructor.
data Button = Button'
    { _bText  :: !Text
    , _bValue :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Button' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bText' - Text visible to the user on the button.
--
-- * 'bValue' - Value sent to Amazon Lex when user clicks the button. For example, consider button text "NYC". When the user clicks the button, the value sent can be "New York City".
button
    :: Text -- ^ 'bText'
    -> Text -- ^ 'bValue'
    -> Button
button pText_ pValue_ =
    Button'
    { _bText = pText_
    , _bValue = pValue_
    }

-- | Text visible to the user on the button.
bText :: Lens' Button Text
bText = lens _bText (\ s a -> s{_bText = a});

-- | Value sent to Amazon Lex when user clicks the button. For example, consider button text "NYC". When the user clicks the button, the value sent can be "New York City".
bValue :: Lens' Button Text
bValue = lens _bValue (\ s a -> s{_bValue = a});

instance FromJSON Button where
        parseJSON
          = withObject "Button"
              (\ x -> Button' <$> (x .: "text") <*> (x .: "value"))

instance Hashable Button

instance NFData Button

-- | Represents an option rendered to the user when a prompt is shown. It could be an image, a button, a link, or text.
--
--
--
-- /See:/ 'genericAttachment' smart constructor.
data GenericAttachment = GenericAttachment'
    { _gaButtons           :: !(Maybe [Button])
    , _gaSubTitle          :: !(Maybe Text)
    , _gaImageURL          :: !(Maybe Text)
    , _gaAttachmentLinkURL :: !(Maybe Text)
    , _gaTitle             :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GenericAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaButtons' - List of options to show to the user.
--
-- * 'gaSubTitle' - Subtitle shown below the title.
--
-- * 'gaImageURL' - URL of an image that is displayed to the user.
--
-- * 'gaAttachmentLinkURL' - Undocumented member.
--
-- * 'gaTitle' - Title of the option.
genericAttachment
    :: GenericAttachment
genericAttachment =
    GenericAttachment'
    { _gaButtons = Nothing
    , _gaSubTitle = Nothing
    , _gaImageURL = Nothing
    , _gaAttachmentLinkURL = Nothing
    , _gaTitle = Nothing
    }

-- | List of options to show to the user.
gaButtons :: Lens' GenericAttachment [Button]
gaButtons = lens _gaButtons (\ s a -> s{_gaButtons = a}) . _Default . _Coerce;

-- | Subtitle shown below the title.
gaSubTitle :: Lens' GenericAttachment (Maybe Text)
gaSubTitle = lens _gaSubTitle (\ s a -> s{_gaSubTitle = a});

-- | URL of an image that is displayed to the user.
gaImageURL :: Lens' GenericAttachment (Maybe Text)
gaImageURL = lens _gaImageURL (\ s a -> s{_gaImageURL = a});

-- | Undocumented member.
gaAttachmentLinkURL :: Lens' GenericAttachment (Maybe Text)
gaAttachmentLinkURL = lens _gaAttachmentLinkURL (\ s a -> s{_gaAttachmentLinkURL = a});

-- | Title of the option.
gaTitle :: Lens' GenericAttachment (Maybe Text)
gaTitle = lens _gaTitle (\ s a -> s{_gaTitle = a});

instance FromJSON GenericAttachment where
        parseJSON
          = withObject "GenericAttachment"
              (\ x ->
                 GenericAttachment' <$>
                   (x .:? "buttons" .!= mempty) <*> (x .:? "subTitle")
                     <*> (x .:? "imageUrl")
                     <*> (x .:? "attachmentLinkUrl")
                     <*> (x .:? "title"))

instance Hashable GenericAttachment

instance NFData GenericAttachment

-- | If you configure a response card when creating your bots, Amazon Lex substitutes the session attributes and slot values available, and then returns it. The response card can also come from a Lambda function ( @dialogCodeHook@ and @fulfillmentActivity@ on an intent).
--
--
--
-- /See:/ 'responseCard' smart constructor.
data ResponseCard = ResponseCard'
    { _rcGenericAttachments :: !(Maybe [GenericAttachment])
    , _rcVersion            :: !(Maybe Text)
    , _rcContentType        :: !(Maybe ContentType)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ResponseCard' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcGenericAttachments' - An array of attachment objects representing options.
--
-- * 'rcVersion' - Version of response card format.
--
-- * 'rcContentType' - Content type of the response.
responseCard
    :: ResponseCard
responseCard =
    ResponseCard'
    { _rcGenericAttachments = Nothing
    , _rcVersion = Nothing
    , _rcContentType = Nothing
    }

-- | An array of attachment objects representing options.
rcGenericAttachments :: Lens' ResponseCard [GenericAttachment]
rcGenericAttachments = lens _rcGenericAttachments (\ s a -> s{_rcGenericAttachments = a}) . _Default . _Coerce;

-- | Version of response card format.
rcVersion :: Lens' ResponseCard (Maybe Text)
rcVersion = lens _rcVersion (\ s a -> s{_rcVersion = a});

-- | Content type of the response.
rcContentType :: Lens' ResponseCard (Maybe ContentType)
rcContentType = lens _rcContentType (\ s a -> s{_rcContentType = a});

instance FromJSON ResponseCard where
        parseJSON
          = withObject "ResponseCard"
              (\ x ->
                 ResponseCard' <$>
                   (x .:? "genericAttachments" .!= mempty) <*>
                     (x .:? "version")
                     <*> (x .:? "contentType"))

instance Hashable ResponseCard

instance NFData ResponseCard
